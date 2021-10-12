@0xc16a95360f09f2fc;

struct Dag {
  using File = Text;
  using DepIndex = UInt16;
  using NodeIndex = UInt32;

  # The first file is always the current file, so beware for loops
  dependencies @0 :List(File);

  nodes @1 :List(Node);
  proofSteps @2 :List(ProofStep);

  struct NodeRef {
    depIndex @0 :DepIndex;
    nodeIndex @1 :NodeIndex;
  }

  struct Node {
    union {
      leaf @0 :LeafKind;
      node :group {
        kind @1 :NodeKind;
        children @2 :List(NodeRef);
      }
    }
  }

  struct ProofStep {
    node @0 :NodeIndex;
    tactic @1 :UInt64;
  }

  struct NodeArity {
    kind @0 :NodeKind;
    arity :union {
      fixed @1 :UInt8;
      variable @2 :Void;
    }
  }

  interface Embedding {
    generateEmbedding @0 (db :List(Node), ps :NodeIndex) -> (emb :List(Float64));
  }

  const globalRefs : List(NodeKind) = [constUndef, constDef, constOpaqueDef, ind, construct, proj];
  const nodeArities : List(NodeArity) =
  [ (kind = withContext, arity = (fixed = 2))
  , (kind = context, arity = (variable = void))
  , (kind = contextDef, arity = (fixed = 2))
  , (kind = contextAssum, arity = (fixed = 1))
  , (kind = constUndef, arity = (fixed = 1))
  , (kind = constDef, arity = (fixed = 2))
  , (kind = constOpaqueDef, arity = (fixed = 2))
  , (kind = ind, arity = (fixed = 2))
  , (kind = indBinder, arity = (fixed = 1))
  , (kind = indConstructs, arity = (variable = void))
  , (kind = construct, arity = (fixed = 1))
  , (kind = proj, arity = (fixed = 1))
  , (kind = evar, arity = (fixed = 2))
  , (kind = evarSubsts, arity = (variable = void))
  , (kind = cast, arity = (fixed = 2))
  , (kind = prod, arity = (fixed = 2))
  , (kind = prodBinder, arity = (fixed = 1))
  , (kind = lambda, arity = (fixed = 2))
  , (kind = lambdaBinder, arity = (fixed = 1))
  , (kind = letIn, arity = (fixed = 2))
  , (kind = letInBinder, arity = (fixed = 2))
  , (kind = app, arity = (fixed = 2))
  , (kind = appArgs, arity = (variable = void))
  , (kind = case, arity = (fixed = 3))
  , (kind = caseBranches, arity = (variable = void))
  , (kind = caseBranch, arity = (fixed = 2))
  , (kind = fix, arity = (fixed = 2))
  , (kind = fixAuxes, arity = (variable = void))
  , (kind = fixFun, arity = (fixed = 2))
  , (kind = fixFunBinder, arity = (fixed = 1))
  , (kind = coFix, arity = (fixed = 2))
  , (kind = coFixAuxes, arity = (variable = void))
  , (kind = coFixFun, arity = (fixed = 2))
  , (kind = coFixFunBinder, arity = (fixed = 1))
  ];

  enum NodeKind {
    # Context
    withContext @0;
    context @1;
    contextDef @2;
    contextAssum @3;

    # Constants
    constUndef @4;
    constDef @5;
    constOpaqueDef @6;

    # Inducives
    ind @7;
    indConstructs @8;
    indBinder @9;
    construct @10;
    proj @11;

    # Constr nodes
    evar @12;
    evarSubsts @13;
    cast @14; # TODO: Do we want cast kind?
    prod @15;
    prodBinder @16;
    lambda @17;
    lambdaBinder @18;
    letIn @19;
    letInBinder @20;
    app @21;
    appArgs @22;
    case @23;
    caseBranches @24;
    caseBranch @25;
    fix @26; # TODO: Recursive var info?
    fixAuxes @27;
    fixFun @28;
    fixFunBinder @29;
    coFix @30;
    coFixAuxes @31;
    coFixFun @32;
    coFixFunBinder @33;
  }

  struct LeafKind {
    union {
      int @0 :UInt64;
      float @1 :Float64;
      constPrimitive @2 :Text; # Finite number of strings # TODO: Should we make the strings explicit?
      sortSProp @3 :Void;
      sortProp @4 :Void;
      sortSet @5 :Void;
      sortType @6 :Void; # Collapsed universe
      evarId @7 :UInt64; # TODO: Should be resolved
      definition :group {
        nameHash @8 :UInt64;
        node @9 :Node;
      }
      cachedDefinition @10 :List(Float32);
    }
  }
}