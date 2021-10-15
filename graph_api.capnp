@0xafda4797418def92;

using File = Text;
using DepIndex = UInt16;
using NodeIndex = UInt32;
using TacticId = UInt64;

struct DirectedEdge {
  source @0 :NodeIndex;
  target :group {
    depIndex @1 :DepIndex;
    nodeIndex @2 :NodeIndex;
  }
}

struct Graph {
  # Gives a classification to every NodeIndex
  classifications @0 :List(Classification);
  edges @1 :List(DirectedEdge);
}

struct ProofState {
  root @0 :NodeIndex;
  context @1 :List(NodeIndex);
}

struct AbstractTactic {
  ident @0 :TacticId;
  parameters @1 :UInt8;
}

struct Tactic {
  ident @0 :TacticId;
  arguments @1 :List(NodeIndex);
}

struct Dataset {
  struct DataPoint {
    state @0 :ProofState;
    tactic @1 :Tactic;
  }
  # The first file is always the current file
  dependencies @0 :List(File);
  graph @1 :Graph;
  proofSteps @2 :List(DataPoint);
}

struct Exception {
  union {
    noSuchTactic @0 :Void;
    mismatchedArguments @1 :Void;
  }
}

struct ExecutionResult {
  union {
    failure @0 :Void;
    complete @1 :Void;
    newState :group {
      graph @2 :Graph;
      state @3 :ProofState;
      obj @4 :ProofObject;
    }
    protocolError @5 :Exception;
  }
}

interface ProofObject {
  runTactic @0 (tactic: Tactic) -> (result: ExecutionResult);
}

interface AvailableTactics {
  tactics @0 () -> (tactics :List(AbstractTactic));
  printTactic @1 (tactic :TacticId) -> (tactic :Text);
}

interface PullReinforce {
  reinforce @0 (lemma :Text) -> (available :AvailableTactics, result :ExecutionResult);
}

interface PushReinforce {
  reinforce @0 (result :ExecutionResult);
  embed @1 (graph :Graph, root :NodeIndex) -> (emb :List(Float64));
}

interface Main {
  initialize @0 (push :PushReinforce) -> (pull :PullReinforce);
}

#const globalRefs :List(Classification) = [const, ind, construct, proj];
struct Classification {
  union {
    root @1 :Void;

    # Local Variable
    localDef @2 :Void;
    localDefType @3 :Void;
    localDefTerm @4 :Void;
    localAssum @5 :Void;

    # Constants
    const @6 :Void;
    constType @7 :Void;
    constUndef @8 :Void;
    constDef @9 :Void;
    constOpaqueDef @10 :Void;
    constPrimitive @11 :Void;

    # Inducives
    ind @12 :Void;
    construct @13 :Void;

    # Sorts
    sort @14 :Void;
    sProp @15 :Void;
    prop @16 :Void;
    set @17 :Void;
    type @18 :Void; # Collapsed universe

    # Constr nodes
    rel @19 :Void;
    var @20 :Void;
    evar @21 :UInt64; # TODO: This could be resolved
    evarSubst @22 :Void;
    cast @23 :Void; # TODO: Do we want cast kind?
    castTerm @24 :Void;
    castType @25 :Void;
    prod @26 :Void;
    prodType @27 :Void;
    prodTerm @28 :Void;
    lambda @29 :Void;
    lambdaType @30 :Void;
    lambdaTerm @31 :Void;
    letIn @32 :Void;
    letInDef @33 :Void;
    letInType @34 :Void;
    letInTerm @35 :Void;
    app @36 :Void;
    appFun @37 :Void;
    appArg @38 :Void;
    case @39 :Void;
    caseTerm @40 :Void;
    caseReturn @41 :Void;
    caseBranch @42 :Void;
    cBConstruct @43 :Void;
    cBTerm @44 :Void;
    fix @45 :Void; # TODO: Recursive var info?
    fixFun @46 :Void;
    fixFunType @47 :Void;
    fixFunTerm @48 :Void;
    fixReturn @49 :Void;
    coFix @50 :Void;
    coFixFun @51 :Void;
    coFixFunType @52 :Void;
    coFixFunTerm @53 :Void;
    coFixReturn @54 :Void;
    proj @55 :Void;

    int @56 :UInt64;
    float @57 :Float64;
    primitive @0 :Text;
  }
}