@0x915afe00e8a4a2e1; # v8

using File = Text;
using DepIndex = UInt16;
using NodeIndex = UInt32;
using TacticId = UInt64;
using DefinitionId = UInt64;
using ProofStateId = UInt32;

struct Graph {
  # Note: This struct fits exactly in 64 bits. Let's keep it that way.
  struct EdgeTarget {
    label @0 :EdgeClassification;
    target :group {
      depIndex @1 :DepIndex;
      nodeIndex @2 :NodeIndex;
    }
  }
  struct Node { # Fits exactly in 128 bits.
    label :union { # Inlined for efficiency purposes
      proofState @0 :Void;
      undefProofState @28 :Void;

      # Context
      contextDef @1 :Text;
      contextAssum @2 :Text;

      # Definitions
      definition @3 :Definition;
      constEmpty @4 :Void;

      # Sorts
      sortSProp @5 :Void;
      sortProp @6 :Void;
      sortSet @7 :Void;
      sortType @8 :Void; # Collapsed universe

      # Constr nodes
      rel @9 :Void;
      evar @10 :Void;
      evarSubst @11 :Void;
      cast @12 :Void;
      prod @13 :Void;
      lambda @14 :Void;
      letIn @15 :Void;
      app @16 :Void;
      appFun @17 :Void;
      appArg @18 :Void;
      case @19 :Void;
      caseBranch @20 :Void;
      fix @21 :Void;
      fixFun @22 :Void;
      coFix @23 :Void;
      coFixFun @24 :Void;

      # Primitives
      int @25 :IntP;
      float @26 :FloatP;
      primitive @27 :Text;
    }

    childrenIndex @29 :UInt32;
    childrenCount @30 :UInt16;
  }
  # The main memory store of the graph. It acts as a heap similar to the main memory of a C/C++ program.
  # The heap is accessed by indexing the `nodes` list using a `NodeIndex` which returns a `Node`.
  # Every node has a label and a list of children, which is indicated as a range within the `edges` list using
  # `childrenIndex` and `childrenCount`. The targets of the edges can again be found in the `nodes` list of the
  # current file or of a dependency.
  # Note that just like in C/C++ doing pointer arithmetic on the heap is undefined behavior, and you may
  # encounter arbitrary garbage if you do this. In particular, iterating over the heap is discouraged.
  # Instead, you should access the heap through various entry-points that are provided.
  nodes @0 :List(Node);
  edges @1 :List(EdgeTarget);
}

struct ProofState {
  root @0 :NodeIndex;
  context @1 :List(NodeIndex);
  text @2 :Text;
  id @3 :ProofStateId;
}

struct AbstractTactic {
  ident @0 :TacticId;
  parameters @1 :UInt8;
}

struct Tactic {

  ident @0 :TacticId;

  text @1 :Text; # WARNING: This is currently not 1-to-1 isomorphic to (ident, arguments)!
  # A textual representation of the base tactic without arguments. It tries to roughly correspond to `ident`.
  # Note, however, that this is a slight under-approximation, because tactic printing is not 100% isomorphic to
  # Coq's internal AST of tactics. As such, there are slightly more unique `ident`'s than `bareText`'s in the dataset.
  baseText @2 :Text;
  intermText @3 :Text;

  # Indicates whether or not `ident` + `arguments` is faithfully reversible into the original "strictified" tactic.
  # Note that this does not necessarily mean that it represents exactly the tactic that was inputted by the user.
  # All tactics are modified to be 'strict' (meaning that tactics that have delayed variables in them break).
  # This flag measures the faithfulness of the representation w.r.t. the strict version of the tactic, not the
  # original tactic inputted by the user.
  exact @4 :Bool;
}

struct Dataset {
  # The first file is always the current file
  dependencies @0 :List(File);
  graph @1 :Graph;

  # The entry point of the global context of definitions that are available when this file is 'Required' by
  # another file. The full global context can be obtained by following the `previous` node of definitions.
  # If the compilation unit does not contain any 'super'-global definitions this is set to `len(graph.nodes)`
  representative @2 :NodeIndex;

  # All of the definitions present in the graph.
  # Note that some of these nodes may not be part of the 'super-global' context that is reachable using the
  # `representative` field as an entry point. The reason is that the global context is a forest (list of tree's)
  # and the 'super-global' context is only the main spine of this forest.
  definitions @3 :List(NodeIndex);
}

struct Exception {
  union {
    noSuchTactic @0 :Void;
    mismatchedArguments @1 :Void;
    parseError @2 :Void;
    illegalArgument @3 :Void;
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
  runTactic @0 (tactic: Tactic, arguments: List(Argument)) -> (result: ExecutionResult);
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
}

struct PredictionProtocol {
  struct Request {
    union {
      # Start a context for making tactical predictions for proof search. The context includes the tactics
      # that are currently available, the definitions that are available. Definitions are currently represented only
      # by their root node, the body of the definition is immediately truncated.
      initialize :group {
        tactics @0 :List(AbstractTactic);
        graph @1 :Graph;
        definitions @2 :List(NodeIndex);
      }
      # Predict a list of tactics given the graph of a proof state. The graph is truncated when it finds a definition.
      # Output is a list of predictions with a confidence. The list is expected to be sorted by decreasing confidence.
      predict :group {
        graph @3 :Graph;
        state @4 :ProofState;
      }
      synchronize @5 :UInt64;
    }
  }
  struct Prediction {
    tactic @0 :Tactic;
    arguments @1 :List(Argument);
    confidence @2 :Float64;
  }
  struct Response {
    union {
      initialized @0 :Void;
      prediction @1 :List(Prediction);
      synchronized @2 :UInt64;
    }
  }
}

struct Argument {
  union {
    unresolvable @0 :Void;
    term :group {
      depIndex @1 :DepIndex;
      nodeIndex @2 :NodeIndex;
    }
  }
}

struct Outcome {
  before @0 :ProofState;
  after @1 :List(ProofState);
  term @2 :NodeIndex;
  termText @3 :Text;

  tacticArguments @4 :List(Argument);
}

struct ProofStep {
  tactic :union {
    unknown @0 :Void;
    known @1 :Tactic;
  }
  outcomes @2 :List(Outcome);
}

struct Definition {
  hash @0 :DefinitionId;
  name @1 :Text;

  # The previous definition within the global context of the current file.
  # For the first definition, the previous node is the node itself, so beware of loops.
  # Attempts are made to make the ordering of the global context consistent with the ordering of definitions
  # in the source document. However, when closing modules and sections this ordering is not guaranteed to be
  # maintained.
  # The contract on this field is that any nodes reachable from the forward closure of the definition must also be
  # reachable through the chain of previous fields. An exception to this rule are mutually recursive definitions.
  # Those nodes are placed into the global context in an arbitrary ordering.
  previous @2 :NodeIndex;

  # This field provides information about the external global context.
  # At any point in a source file other files 'X' can be loaded through 'Require X'. When this happens, the
  # definitions of X that are reachable through its 'representative' field become available to all subsequent
  # definitions.
  externalPrevious @3 :List(DepIndex);

  # A definition is either
  # (1) an object as originally inputted by the user
  # (2) a definition that was originally defined in a section and has now had the section
  #     variables discharged into it.
  # (3) a definition that was obtained by performing some sort of module functor substitution.
  # When a definition is not original, we cross-reference to the definition that it was derived from.
  status :union {
    original @4 :Void;
    discharged @5 :NodeIndex;
    substituted :group {
      depIndex @6 :DepIndex;
      nodeIndex @7 :NodeIndex;
    }
  }

  union {
    inductive @8 :Void;
    constructor @9 :Void;
    projection @10 :Void;

    # A constant defined by directly inputting a term
    # In the future, we might augment such constants with tactical
    # refinement proofs that build the term iteratively.
    manualConstant @11 :Void;

    # A constant that was either directly or indirectly generated by a tactical proof.
    # Note that for non-original constants, the proof step sequence may not be very accurate.
    tacticalConstant @12 :List(ProofStep);

    manualSectionConstant @13 :Void;
    tacticalSectionConstant @14 :List(ProofStep);
  }
}

# Used for in-mermory space optimization. This allows us to make structs smaller by reusing space in
# the pointer section of a struct that would otherwise be allocated in the data section.
struct FloatP {
  value @0 :Float64;
}
struct IntP {
  value @0 :UInt64;
}


enum EdgeClassification {
  # Contexts
  contextElem @0;
  contextSubject @1;

  # Context elements
  contextDefType @2;
  contextDefTerm @3;

  # Constants
  constType @4;
  constUndef @5;
  constDef @6;
  constOpaqueDef @7;
  constPrimitive @8;

  # Inductives
  indType @9;
  indConstruct @10;
  projTerm @11;
  constructTerm @12;

  # Casts
  castTerm @13;
  castType @14;

  # Products
  prodType @15;
  prodTerm @16;

  # Lambdas
  lambdaType @17;
  lambdaTerm @18;

  # LetIns
  letInDef @19;
  letInType @20;
  letInTerm @21;

  # Apps
  appFunPointer @22;
  appFunValue @23;
  appArgPointer @24;
  appArgValue @25;
  appArgOrder @26;

  # Cases
  caseTerm @27;
  caseReturn @28;
  caseBranchPointer @29;
  caseInd @30;

  # CaseBranches
  cBConstruct @31;
  cBTerm @32;

  # Fixes
  fixMutual @33;
  fixReturn @34;

  # FixFuns
  fixFunType @35;
  fixFunTerm @36;

  # CoFixes
  coFixMutual @37;
  coFixReturn @38;

  # CoFixFuns
  coFixFunType @39;
  coFixFunTerm @40;

  # Back pointers
  relPointer @41;

  # Evars
  evarSubstPointer @42;
  evarSubstTerm @43;
  evarSubstTarget @44;
  evarSubject @45;
}

# Struct is needed to work around
# https://github.com/capnproto/capnp-ocaml/issues/81
struct ConflatableEdges {
  conflatable @0 :List(EdgeClassification);
}
const conflatableEdges :List(ConflatableEdges) =
[ ( conflatable = [contextDefType, constType, indType, castType, prodType, lambdaType, letInType, fixFunType, coFixFunType] )
, ( conflatable = [contextDefTerm, castTerm, prodTerm, lambdaTerm, letInTerm, fixFunTerm, coFixFunTerm] )
# Not conflatable: projTerm, constructTerm, caseTerm, cBTerm
];
const importantEdges :List(EdgeClassification) =
[ contextElem, contextSubject, contextDefType, contextDefTerm, constType, constDef, constOpaqueDef, indType, indConstruct, constructTerm
, prodType, prodTerm, lambdaType, lambdaTerm, letInDef, letInType, letInTerm, appFunPointer, appArgPointer, appArgOrder, relPointer ];
const lessImportantEdges :List(EdgeClassification) =
[ caseTerm, caseReturn, caseBranchPointer, caseInd, cBConstruct, cBTerm, fixMutual, fixReturn, fixFunType, fixFunTerm ];
const leastImportantEdges :List(EdgeClassification) =
[ constUndef, constPrimitive, projTerm, castTerm, castType, appFunValue, appArgValue, coFixReturn, coFixFunType, coFixFunTerm
, coFixMutual, evarSubstPointer, evarSubstTerm, evarSubstTarget ];

# WARNING: DO NOT USE
# This is just for visualization purposes in order to drastically reduce the number of edges. You should not use it in networks
const groupedEdges :List(ConflatableEdges) =
[ ( conflatable = [contextElem, contextSubject] )
, ( conflatable = [contextDefType, contextDefTerm] )
, ( conflatable = [constType, constUndef, constDef, constOpaqueDef, constPrimitive] )
, ( conflatable = [indType, indConstruct] )
, ( conflatable = [projTerm] )
, ( conflatable = [constructTerm] )
, ( conflatable = [castType, castTerm] )
, ( conflatable = [prodType, prodTerm] )
, ( conflatable = [lambdaType, lambdaTerm] )
, ( conflatable = [letInDef, letInTerm, letInType] )
, ( conflatable = [appFunPointer, appArgPointer, appArgOrder] )
, ( conflatable = [appFunValue] )
, ( conflatable = [appArgValue] )
, ( conflatable = [caseTerm, caseReturn, caseBranchPointer, caseInd] )
, ( conflatable = [cBConstruct, cBTerm] )
, ( conflatable = [fixMutual, fixReturn] )
, ( conflatable = [fixFunType, fixFunTerm] )
, ( conflatable = [coFixMutual, coFixReturn] )
, ( conflatable = [coFixFunType, coFixFunTerm] )
, ( conflatable = [relPointer] )
, ( conflatable = [evarSubject, evarSubstPointer] )
, ( conflatable = [evarSubstTerm, evarSubstTarget] )
];
