@0xafda4797418def92;

using File = Text;
using DepIndex = UInt16;
using NodeIndex = UInt32;
using TacticId = UInt64;
using DefinitionId = UInt64;

struct DirectedEdge {
  source @0 :NodeIndex;
  sort @1 :EdgeClassification;
  target :group {
    depIndex @2 :DepIndex;
    nodeIndex @3 :NodeIndex;
  }
}

struct Graph {
  # Gives a classification to every NodeIndex
  classifications @0 :List(NodeClassification);
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
    parseError @2 :Void;
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

# const globalRefs :List(NodeClassification) = [const, ind, construct, proj];
struct NodeClassification {
  union {
    root @0 :Void;

    # Context
    contextDef @1 :Void;
    contextAssum @2 :Void;

    # Constants
    const @3 :DefinitionId;
    constEmpty @4 :Void;

    # Inductives
    ind @5 :DefinitionId;
    construct @6 :DefinitionId;
    proj @7 :DefinitionId;

    # Sorts
    sortSProp @8 :Void;
    sortProp @9 :Void;
    sortSet @10 :Void;
    sortType @11 :Void; # Collapsed universe

    # Constr nodes
    rel @12 :Void;
    var @13 :Void;
    evar @14 :UInt64;
    evarSubst @15 :Void;
    cast @16 :Void;
    prod @17 :Void;
    lambda @18 :Void;
    letIn @19 :Void;
    app @20 :Void;
    appFun @21 :Void;
    appArg @22 :Void;
    case @23 :Void;
    caseBranch @24 :Void;
    fix @25 :Void;
    fixFun @26 :Void;
    coFix @27 :Void;
    coFixFun @28 :Void;

    # Primitives
    int @29 :UInt64;
    float @30 :Float64;
    primitive @31 :Text;
  }
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
, ( conflatable = [varPointer, relPointer] )
, ( conflatable = [appArgOrder, evarSubstOrder] )
];
const importantEdges :List(EdgeClassification) =
[ contextElem, contextSubject, contextDefType, contextDefTerm, constType, constDef, constOpaqueDef, indType, indConstruct, constructTerm
, prodType, prodTerm, lambdaType, lambdaTerm, letInDef, letInType, letInTerm, appFunPointer, appArgPointer, appArgOrder, relPointer, varPointer ];
const lessImportantEdges :List(EdgeClassification) =
[ caseTerm, caseReturn, caseBranchPointer, caseInd, cBConstruct, cBTerm, fixMutual, fixReturn, fixFunType, fixFunTerm ];
const leastImportantEdges :List(EdgeClassification) =
[ constUndef, constPrimitive, projTerm, castTerm, castType, appFunValue, appArgValue, coFixReturn, coFixFunType, coFixFunTerm
, coFixMutual, evarSubstPointer, evarSubstOrder, evarSubstValue ];
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

  # Constr edges
  relPointer @41;
  varPointer @42;
  evarSubstPointer @43;
  evarSubstOrder @44;
  evarSubstValue @45;
}

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
, ( conflatable = [varPointer] )
, ( conflatable = [evarSubstPointer] )
, ( conflatable = [evarSubstOrder, evarSubstValue] )
];
