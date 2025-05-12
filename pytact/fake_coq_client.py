import contextlib
import socket
import argparse
import signal
from subprocess import Popen
import capnp
import pytact.graph_api_capnp as graph_api_capnp
from pytact.data_reader import capnp_message_generator_from_file_lowlevel, record_lowlevel_generator

def run_fake_client(server_socket, messages_generator):
    socket_reader = graph_api_capnp.PredictionProtocol.Response.read_multiple_packed(
        server_socket, traversal_limit_in_words=2**64-1)
    for msg in messages_generator:
        msg.dynamic.as_builder().write_packed(server_socket)
        prev_sig = signal.signal(signal.SIGINT, signal.SIG_DFL)  # SIGINT catching OFF
        response = next(socket_reader)
        signal.signal(signal.SIGINT, prev_sig)  # SIGINT catching ON
        messages_generator.send(response.as_builder())

def compare(request, response, recorded_response):
    if response.to_dict() == recorded_response.to_dict():
        print(f'The servers response to a {request.which.name} message was equal to the '
              f'recorded response')
    else:
        raise ValueError(
            f"The servers response to a {request.which.name} message was not equal to the " +
            f"recorded response.\n"
            f"Recorded response: {recorded_response}\n"
            f"Servers response: {response}\n"
        )

def main():
    parser = argparse.ArgumentParser(
        description = 'A fake Coq client that connects to a prediction server and feeds it a stream of previously ' +
                      'recorded messages.',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('data',
                        type=str,
                        help='A file that contains a previously recorded communication sequence between Coq ' +
                             'and a prediction server.')
    parser.add_argument('--check',
                        action=argparse.BooleanOptionalAction,
                        default = True,
                        help='Wether or not to compare the response messages of the server to recorded messages.')
    parser.add_argument('--record',
                        dest="record_file",
                        type = str,
                        default = None,
                        help='Re-record the interaction with the server to a new file (useful when you want to ' +
                    'update the trace with a new response behavior of a server)')
    connect_group = parser.add_mutually_exclusive_group(required=True)
    connect_group.add_argument('--tcp',
                        dest='tcp_location',
                        type = str,
                        default = None,
                        help='Connect to a prediction server on the specified ip:port.')
    connect_group.add_argument('--stdin',
                        dest='executable',
                        type = str,
                        default = None,
                        help='Start the specified prediction server and connect to it through stdin.')
    cmd_args = parser.parse_args()

    with open(cmd_args.data, 'rb') as message_file:
        messages_generator = capnp_message_generator_from_file_lowlevel(
            message_file,
            check= compare if cmd_args.check else None)
        if cmd_args.record_file is not None:
            record_context = open(cmd_args.record_file, 'wb')
        else:
            record_context = contextlib.nullcontext()
        with record_context as record_file:
            if record_file is not None:
                messages_generator = record_lowlevel_generator(record_file, messages_generator)
            if cmd_args.tcp_location is not None:
                addr, port = cmd_args.tcp_location.split(':')
                with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as server_socket:
                    server_socket.connect((addr, int(port)))
                    run_fake_client(server_socket, messages_generator)
            else:
                our_sock, their_sock = socket.socketpair()
                process = Popen(cmd_args.executable, shell=True, stdin=their_sock)
                their_sock.close()
                run_fake_client(our_sock, messages_generator)
                our_sock.close()
                process.communicate()

if __name__ == '__main__':
    main()
