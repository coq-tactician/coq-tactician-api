# distutils: language = c++
# distutils: libraries = capnpc capnp capnp-rpc
# distutils: sources = pytact/graph_api.capnp.cpp
# cython: c_string_type = str
# cython: c_string_encoding = default
# cython: embedsignature = True
# cython: language_level = 3

import os
import capnp
import pytact.graph_api_capnp

cdef class Graph_Node_Reader:
    """
    Fits exactly in 128 bits.
    A node has a label that optionally contains some additional information, together with a list of
    outgoing edges (children).
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asGraph_Node()

    @staticmethod
    cdef init(C_Graph_Node_Reader source):
        cdef Graph_Node_Reader wrapper = Graph_Node_Reader.__new__(Graph_Node_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def label(self):
        """
        Inlined for efficiency purposes
        """
        return Graph_Node_Label_Reader.init(self.source.getLabel())

    @property
    def children_index(self):
        return self.source.getChildrenIndex()

    @property
    def children_count(self):
        """
        The children of a node are encoded as a range withing the `edges`-list of the graph.
        """
        return self.source.getChildrenCount()

    @property
    def identity(self):
        """
        The identity of a node uniquely determines it. That is, one can consider any to nodes with the same
        identity to be equal. The notion of equal we use is as follows:
        1. Graph perspective: Two nodes have the same identity if and only if they are bisimilar.
           In this notion, bisimilarity does take into consideration the label of the nodes, modulo some
           equivalence class that is not fully specified here. One aspect of the equivalence class is that
           for definition nodes their associated global context (accessed through `Definition.previous`) is
           not taken into account.
        2. Lambda calculus perspective: Two nodes have the same identity if their corresponding lambda terms
           are alpha-equivalent. Note that two definitions with the same body are not considered
           alpha-equivalent.

        The identity of a node is used to perform partial graph-sharing. That is, two nodes with the same
        identity are merged when the graph is generated. There are two reasons why two nodes with the same
        semantic identity might have a different physical identity:
        1. Nodes are only merged when the first node exists in the same graph as the second node, or exists
           in a dependent graph. Hence, nodes originating from developments that do not depend on each other
           are never merged. Full graph-sharing would require global analysis on a dataset, which any consumer
           can optionally do as a post-processing step.
        2. Two definition nodes with the same name and body have the same identity. But if they occur in
           different global contexts, these nodes are physically different to ensure the integrity of their
           global contexts.

        Beware that the identity is currently a 64 bit field. For datasets that have graphs of size in the
        order of billions of nodes there is a non-trivial chance of a collision. (We consider this acceptable
        for machine learning purposes.)
        """
        return self.source.getIdentity()
pytact.graph_api_capnp.Graph.Node.schema = _Schema()._init(getGraph_Node_Schema()).as_struct()

cdef class Graph_EdgeTarget_Reader:
    """
    Fits exactly in 64 bits. Let's keep it that way.
    The 'pointy' end of an edge, together with the label of that edge.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asGraph_EdgeTarget()

    @staticmethod
    cdef init(C_Graph_EdgeTarget_Reader source):
        cdef Graph_EdgeTarget_Reader wrapper = Graph_EdgeTarget_Reader.__new__(Graph_EdgeTarget_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def label(self):
        return None(self.source.getLabel())

    @property
    def target(self):
        return Graph_EdgeTarget_Target_Reader.init(self.source.getTarget())
pytact.graph_api_capnp.Graph.EdgeTarget.schema = _Schema()._init(getGraph_EdgeTarget_Schema()).as_struct()

cdef class Graph_Reader:
    """
    A graph is the main data store (the 'heap') that contains the bulk of the data in the dataset and
    during communication with Coq. A graph is a collection of labeled nodes with directed, labeled edges
    between them. A graph may reference nodes from other graphs. For an edge 'A --> B' in the graph it
    always holds that 'A' is part of the current graph, but 'B' may (or may not) be part of another graph.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asGraph()

    @staticmethod
    cdef init(C_Graph_Reader source):
        cdef Graph_Reader wrapper = Graph_Reader.__new__(Graph_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def nodes(self):
        return Graph_Node_Reader_List.init(self.source.getNodes())
    @property
    def has_nodes(self):
        return self.source.hasNodes()

    @property
    def edges(self):
        """
        The main memory store of the graph. It acts as a heap similar to the main memory of a C/C++ program.
        The heap is accessed by indexing the `nodes` list using a `NodeIndex` which returns a `Node`.
        Every node has a label and a list of children, which is indicated as a range within the `edges` list using
        `childrenIndex` and `childrenCount`. The targets of the edges can again be found in the `nodes` list of the
        current file or of a dependency.
        Note that just like in C/C++ doing pointer arithmetic on the heap is undefined behavior, and you may
        encounter arbitrary garbage if you do this. In particular, iterating over the heap is discouraged.
        Instead, you should access the heap through various entry-points that are provided.
        """
        return Graph_EdgeTarget_Reader_List.init(self.source.getEdges())
    @property
    def has_edges(self):
        return self.source.hasEdges()
pytact.graph_api_capnp.Graph.schema = _Schema()._init(getGraph_Schema()).as_struct()

cdef class Definition_Reader:
    """
    A definition of the CIC, which is either an constant, inductive, constructor, projection or section
    variable. Constants and section variables can have tactical proofs associated to them.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asDefinition()

    @staticmethod
    cdef init(C_Definition_Reader source):
        cdef Definition_Reader wrapper = Definition_Reader.__new__(Definition_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)
    @property
    def which(self):
        return Definition_Which(self.source.which())
    @property
    def which_raw(self):
        return self.source.which()

    @property
    def name(self):
        """
        The fully-qualified name of the definition. The name should be unique in a particular global context,
        but is not unique among different branches of the global in a dataset.
        """
        temp = self.source.getName()
        return (<char*>temp.begin())[:temp.size()]
    @property
    def has_name(self):
        return self.source.hasName()

    @property
    def previous(self):
        """
        The previous definition within the global context of the current file.
        For the first definition this field is set to `len(graph.nodes)`.
        The contract on this field is that any definition nodes reachable from the forward closure of the definition
        must also be reachable through the chain of previous fields. An exception to this rule are mutually
        recursive definitions. Those nodes are placed into the global context in an arbitrary ordering.
        """
        return self.source.getPrevious()

    @property
    def external_previous(self):
        """
        This field provides information about the external global context.
        At any point in a source file other files 'X' can be loaded through 'Require X'. When this happens, the
        definitions of X that are reachable through its 'representative' field become available to all subsequent
        definitions.
        """
        return Uint16_List.init(self.source.getExternalPrevious())
    @property
    def has_external_previous(self):
        return self.source.hasExternalPrevious()

    @property
    def status(self):
        """
        A definition is either
        (1) an object as originally inputted by the user.
        (2) a definition that was originally defined in a section and has now had the section
            variables discharged into it.
        (3) a definition that was obtained by performing some sort of module functor substitution.
        When a definition is not original, we cross-reference to the definition that it was derived from.
        """
        return Definition_Status_Reader.init(self.source.getStatus())

    @property
    def inductive(self):
        return self.source.getInductive()
    @property
    def is_inductive(self):
        return self.source.isInductive()

    @property
    def constructor(self):
        return self.source.getConstructor()
    @property
    def is_constructor(self):
        return self.source.isConstructor()

    @property
    def projection(self):
        """
        These definitions are part of a mutually recursive cluster. They hold a reference to another definition
        that acts as the 'representative' of the mutually recursive cluster. The representative is chosen such
        that all definitions of the cluster are reachable through its `previous` pointer. Additionally, all
        definitions within the cluster have the same representative, and no definitions that are not part of the
        cluster are interleaved within the chain of `previous` nodes.
        """
        return self.source.getProjection()
    @property
    def is_projection(self):
        return self.source.isProjection()

    @property
    def manual_constant(self):
        """
        A constant defined by directly inputting a term
        In the future, we might augment such constants with tactical
        refinement proofs that build the term iteratively.
        """
        return self.source.getManualConstant()
    @property
    def is_manual_constant(self):
        return self.source.isManualConstant()

    @property
    def tactical_constant(self):
        """
        A constant that was either directly or indirectly generated by a tactical proof.
        Note that for non-original constants, the proof step sequence may not be very accurate.
        """
        return ProofStep_Reader_List.init(self.source.getTacticalConstant())
    @property
    def is_tactical_constant(self):
        return self.source.isTacticalConstant()
    @property
    def has_tactical_constant(self):
        return self.source.hasTacticalConstant()

    @property
    def manual_section_constant(self):
        """
        A section variable or local section definition.
        """
        return self.source.getManualSectionConstant()
    @property
    def is_manual_section_constant(self):
        return self.source.isManualSectionConstant()

    @property
    def tactical_section_constant(self):
        """
        Local section definitions can also be defined using a tactical proof.
        """
        return ProofStep_Reader_List.init(self.source.getTacticalSectionConstant())
    @property
    def is_tactical_section_constant(self):
        return self.source.isTacticalSectionConstant()
    @property
    def has_tactical_section_constant(self):
        return self.source.hasTacticalSectionConstant()

    @property
    def type_text(self):
        """
        A textual representation of the type of this definition.
        """
        temp = self.source.getTypeText()
        return (<char*>temp.begin())[:temp.size()]
    @property
    def has_type_text(self):
        return self.source.hasTypeText()

    @property
    def term_text(self):
        """
        A textual representation of the body of this definition.
        For inductives, constructors, projections, section variables and axioms the string is empty.
        """
        temp = self.source.getTermText()
        return (<char*>temp.begin())[:temp.size()]
    @property
    def has_term_text(self):
        return self.source.hasTermText()
pytact.graph_api_capnp.Definition.schema = _Schema()._init(getDefinition_Schema()).as_struct()

cdef class IntP_Reader:

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asIntP()

    @staticmethod
    cdef init(C_IntP_Reader source):
        cdef IntP_Reader wrapper = IntP_Reader.__new__(IntP_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def value(self):
        return self.source.getValue()
pytact.graph_api_capnp.IntP.schema = _Schema()._init(getIntP_Schema()).as_struct()

cdef class FloatP_Reader:

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asFloatP()

    @staticmethod
    cdef init(C_FloatP_Reader source):
        cdef FloatP_Reader wrapper = FloatP_Reader.__new__(FloatP_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def value(self):
        return self.source.getValue()
pytact.graph_api_capnp.FloatP.schema = _Schema()._init(getFloatP_Schema()).as_struct()

cdef class Graph_Node_Label_Reader:
    """
    Inlined for efficiency purposes
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asGraph_Node_Label()

    @staticmethod
    cdef init(C_Graph_Node_Label_Reader source):
        cdef Graph_Node_Label_Reader wrapper = Graph_Node_Label_Reader.__new__(Graph_Node_Label_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)
    @property
    def which(self):
        return Graph_Node_Label_Which(self.source.which())
    @property
    def which_raw(self):
        return self.source.which()

    @property
    def proof_state(self):
        return self.source.getProofState()
    @property
    def is_proof_state(self):
        return self.source.isProofState()

    @property
    def context_def(self):
        return self.source.getContextDef()
    @property
    def is_context_def(self):
        return self.source.isContextDef()

    @property
    def context_assum(self):
        return self.source.getContextAssum()
    @property
    def is_context_assum(self):
        return self.source.isContextAssum()

    @property
    def definition(self):
        return Definition_Reader.init(self.source.getDefinition())
    @property
    def is_definition(self):
        return self.source.isDefinition()
    @property
    def has_definition(self):
        return self.source.hasDefinition()

    @property
    def const_empty(self):
        return self.source.getConstEmpty()
    @property
    def is_const_empty(self):
        return self.source.isConstEmpty()

    @property
    def sort_s_prop(self):
        return self.source.getSortSProp()
    @property
    def is_sort_s_prop(self):
        return self.source.isSortSProp()

    @property
    def sort_prop(self):
        return self.source.getSortProp()
    @property
    def is_sort_prop(self):
        return self.source.isSortProp()

    @property
    def sort_set(self):
        return self.source.getSortSet()
    @property
    def is_sort_set(self):
        return self.source.isSortSet()

    @property
    def sort_type(self):
        """
        Collapsed universe
        """
        return self.source.getSortType()
    @property
    def is_sort_type(self):
        return self.source.isSortType()

    @property
    def rel(self):
        return self.source.getRel()
    @property
    def is_rel(self):
        return self.source.isRel()

    @property
    def evar(self):
        return self.source.getEvar()
    @property
    def is_evar(self):
        return self.source.isEvar()

    @property
    def evar_subst(self):
        return self.source.getEvarSubst()
    @property
    def is_evar_subst(self):
        return self.source.isEvarSubst()

    @property
    def cast(self):
        return self.source.getCast()
    @property
    def is_cast(self):
        return self.source.isCast()

    @property
    def prod(self):
        return self.source.getProd()
    @property
    def is_prod(self):
        return self.source.isProd()

    @property
    def lambda_(self):
        return self.source.getLambda()
    @property
    def is_lambda_(self):
        return self.source.isLambda()

    @property
    def let_in(self):
        return self.source.getLetIn()
    @property
    def is_let_in(self):
        return self.source.isLetIn()

    @property
    def app(self):
        return self.source.getApp()
    @property
    def is_app(self):
        return self.source.isApp()

    @property
    def case(self):
        return self.source.getCase()
    @property
    def is_case(self):
        return self.source.isCase()

    @property
    def case_branch(self):
        return self.source.getCaseBranch()
    @property
    def is_case_branch(self):
        return self.source.isCaseBranch()

    @property
    def fix(self):
        return self.source.getFix()
    @property
    def is_fix(self):
        return self.source.isFix()

    @property
    def fix_fun(self):
        return self.source.getFixFun()
    @property
    def is_fix_fun(self):
        return self.source.isFixFun()

    @property
    def co_fix(self):
        return self.source.getCoFix()
    @property
    def is_co_fix(self):
        return self.source.isCoFix()

    @property
    def co_fix_fun(self):
        return self.source.getCoFixFun()
    @property
    def is_co_fix_fun(self):
        return self.source.isCoFixFun()

    @property
    def int(self):
        return IntP_Reader.init(self.source.getInt())
    @property
    def is_int(self):
        return self.source.isInt()
    @property
    def has_int(self):
        return self.source.hasInt()

    @property
    def float(self):
        return FloatP_Reader.init(self.source.getFloat())
    @property
    def is_float(self):
        return self.source.isFloat()
    @property
    def has_float(self):
        return self.source.hasFloat()

    @property
    def primitive(self):
        temp = self.source.getPrimitive()
        return (<char*>temp.begin())[:temp.size()]
    @property
    def is_primitive(self):
        return self.source.isPrimitive()
    @property
    def has_primitive(self):
        return self.source.hasPrimitive()

    @property
    def undef_proof_state(self):
        return self.source.getUndefProofState()
    @property
    def is_undef_proof_state(self):
        return self.source.isUndefProofState()

cdef class Definition_Status_Substituted_Reader:

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asDefinition_Status_Substituted()

    @staticmethod
    cdef init(C_Definition_Status_Substituted_Reader source):
        cdef Definition_Status_Substituted_Reader wrapper = Definition_Status_Substituted_Reader.__new__(Definition_Status_Substituted_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def dep_index(self):
        return self.source.getDepIndex()

    @property
    def node_index(self):
        return self.source.getNodeIndex()

cdef class Definition_Status_Reader:
    """
    A definition is either
    (1) an object as originally inputted by the user.
    (2) a definition that was originally defined in a section and has now had the section
        variables discharged into it.
    (3) a definition that was obtained by performing some sort of module functor substitution.
    When a definition is not original, we cross-reference to the definition that it was derived from.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asDefinition_Status()

    @staticmethod
    cdef init(C_Definition_Status_Reader source):
        cdef Definition_Status_Reader wrapper = Definition_Status_Reader.__new__(Definition_Status_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)
    @property
    def which(self):
        return Definition_Status_Which(self.source.which())
    @property
    def which_raw(self):
        return self.source.which()

    @property
    def original(self):
        return self.source.getOriginal()
    @property
    def is_original(self):
        return self.source.isOriginal()

    @property
    def discharged(self):
        return self.source.getDischarged()
    @property
    def is_discharged(self):
        return self.source.isDischarged()

    @property
    def substituted(self):
        return Definition_Status_Substituted_Reader.init(self.source.getSubstituted())
    @property
    def is_substituted(self):
        return self.source.isSubstituted()

cdef class ProofStep_Reader:
    """
    A proof step is the execution of a single tactic on one or more proof states, producing a list of outcomes.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asProofStep()

    @staticmethod
    cdef init(C_ProofStep_Reader source):
        cdef ProofStep_Reader wrapper = ProofStep_Reader.__new__(ProofStep_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def tactic(self):
        """
        The tactic that generated the proof step. Note that the arguments of the tactic can be found in the
        individual outcomes, because they may be different for each outcome.
        """
        return ProofStep_Tactic_Reader.init(self.source.getTactic())

    @property
    def outcomes(self):
        """
        A list of transformations of proof states to other proof states, as executed by the tactic of the proof step
        """
        return Outcome_Reader_List.init(self.source.getOutcomes())
    @property
    def has_outcomes(self):
        return self.source.hasOutcomes()
pytact.graph_api_capnp.ProofStep.schema = _Schema()._init(getProofStep_Schema()).as_struct()

cdef class Tactic_Reader:
    """
    A concrete tactic with it's parameters determined. Somewhat strangely, this struct does not actually include
    these parameters. They can instead be found in `Outcome.tacticArguments`. The reason for this is that one
    tactic can run on multiple proof states at the same time and for all of those proof states, the arguments
    may be resolved differently.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asTactic()

    @staticmethod
    cdef init(C_Tactic_Reader source):
        cdef Tactic_Reader wrapper = Tactic_Reader.__new__(Tactic_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def ident(self):
        """
        A hash representing the identity of a tactic without it's arguments. Due to the complexity of the syntax
        trees of Coq's tactics, we do not currently encode the syntax tree. Instead, this hash is a representative
        of the syntax tree of the tactic with all of it's arguments removed.
        """
        return self.source.getIdent()

    @property
    def text(self):
        """
        The full text of the tactic including the full arguments. This does not currently correspond to
        (ident, arguments) because in this dataset arguments do not include full terms, but only references to
        definitions and local context elements.
        """
        temp = self.source.getText()
        return (<char*>temp.begin())[:temp.size()]
    @property
    def has_text(self):
        return self.source.hasText()

    @property
    def base_text(self):
        """
        A textual representation of the base tactic without arguments. It tries to roughly correspond to `ident`.
        Note, however, that this is both an under-approximation and an over-approximation. The reason is that tactic
        printing is not 100% isomorphic to Coq's internal AST of tactics. Sometimes, different tactics get mapped to
        the same text. Conversely, the same tactic may be mapped to different texts when identifiers are printed
        using different partially-qualified names.
        """
        temp = self.source.getBaseText()
        return (<char*>temp.begin())[:temp.size()]
    @property
    def has_base_text(self):
        return self.source.hasBaseText()

    @property
    def interm_text(self):
        """
        A textual representation that tries to come as close as possible to (ident, arguments).
        It comes with the same caveats as `baseText`.
        """
        temp = self.source.getIntermText()
        return (<char*>temp.begin())[:temp.size()]
    @property
    def has_interm_text(self):
        return self.source.hasIntermText()

    @property
    def exact(self):
        """
        Indicates whether or not `ident` + `arguments` is faithfully reversible into the original "strictified" tactic.
        Note that this does not necessarily mean that it represents exactly the tactic that was inputted by the user.
        All tactics are modified to be 'strict' (meaning that tactics that have delayed variables in them break).
        This flag measures the faithfulness of the representation w.r.t. the strict version of the tactic, not the
        original tactic inputted by the user.
        """
        return self.source.getExact()
pytact.graph_api_capnp.Tactic.schema = _Schema()._init(getTactic_Schema()).as_struct()

cdef class ProofStep_Tactic_Reader:
    """
    The tactic that generated the proof step. Note that the arguments of the tactic can be found in the
    individual outcomes, because they may be different for each outcome.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asProofStep_Tactic()

    @staticmethod
    cdef init(C_ProofStep_Tactic_Reader source):
        cdef ProofStep_Tactic_Reader wrapper = ProofStep_Tactic_Reader.__new__(ProofStep_Tactic_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)
    @property
    def which(self):
        return ProofStep_Tactic_Which(self.source.which())
    @property
    def which_raw(self):
        return self.source.which()

    @property
    def unknown(self):
        """
        Sometimes a tactic cannot or should not be recorded. In those cases, it is marked as 'unknown'.
        This currently happens with tactics that are run as a result of the `Proof with tac` construct and it
        happens for tactics that are known to be unsafe like `change_no_check`, `fix`, `cofix` and more.
        """
        return self.source.getUnknown()
    @property
    def is_unknown(self):
        return self.source.isUnknown()

    @property
    def known(self):
        return Tactic_Reader.init(self.source.getKnown())
    @property
    def is_known(self):
        return self.source.isKnown()
    @property
    def has_known(self):
        return self.source.hasKnown()

cdef class Outcome_Reader:
    """
    An outcome is the result of running a tactic on a proof state. A tactic may run on multiple proof states.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asOutcome()

    @staticmethod
    cdef init(C_Outcome_Reader source):
        cdef Outcome_Reader wrapper = Outcome_Reader.__new__(Outcome_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def before(self):
        """
        The proof state before the tactic execution.
        """
        return ProofState_Reader.init(self.source.getBefore())
    @property
    def has_before(self):
        return self.source.hasBefore()

    @property
    def after(self):
        """
        The new proof states that were generated by the tactic.
        """
        return ProofState_Reader_List.init(self.source.getAfter())
    @property
    def has_after(self):
        return self.source.hasAfter()

    @property
    def term(self):
        """
        The proof term that witnesses the transition from the before state to the after states. It contains a hole
        (an `evar` node) for each of the after states. It may also refer to elements of the local context of the
        before state.
        """
        return Outcome_Term_Reader.init(self.source.getTerm())

    @property
    def term_text(self):
        """
        A textual representation of the proof term.
        """
        temp = self.source.getTermText()
        return (<char*>temp.begin())[:temp.size()]
    @property
    def has_term_text(self):
        return self.source.hasTermText()

    @property
    def tactic_arguments(self):
        """
        The arguments of the tactic that produced this outcome. Note that these arguments belong to the tactic in
        `ProofStep.tactic`.
        """
        return Argument_Reader_List.init(self.source.getTacticArguments())
    @property
    def has_tactic_arguments(self):
        return self.source.hasTacticArguments()
pytact.graph_api_capnp.Outcome.schema = _Schema()._init(getOutcome_Schema()).as_struct()

cdef class Outcome_Term_Reader:
    """
    The proof term that witnesses the transition from the before state to the after states. It contains a hole
    (an `evar` node) for each of the after states. It may also refer to elements of the local context of the
    before state.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asOutcome_Term()

    @staticmethod
    cdef init(C_Outcome_Term_Reader source):
        cdef Outcome_Term_Reader wrapper = Outcome_Term_Reader.__new__(Outcome_Term_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def dep_index(self):
        return self.source.getDepIndex()

    @property
    def node_index(self):
        return self.source.getNodeIndex()

cdef class ProofState_Reader:
    """
    A proof state represents a particular point in the tactical proof of a constant.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asProofState()

    @staticmethod
    cdef init(C_ProofState_Reader source):
        cdef ProofState_Reader wrapper = ProofState_Reader.__new__(ProofState_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def root(self):
        """
        The entry-point of the proof state, all nodes that are 'part of' the proof state are reachable from here.
        """
        return ProofState_Root_Reader.init(self.source.getRoot())

    @property
    def context(self):
        """
        The local context of the proof state. These nodes label's are either `contextAssum` or `contextDef`. Note that
        these nodes are also reachable from the root of the proof state.
        """
        return Node_Reader_List.init(self.source.getContext())
    @property
    def has_context(self):
        return self.source.hasContext()

    @property
    def context_names(self):
        """
        The names of the local context nodes of the proof state, as they originally appeared in the proof.
        These names should be used for debugging and viewing purposes only, because hypothesis-generating tactics have
        been modified to use auto-generated names. Hence, tactics should not be concerned about the names of
        the context.
        """
        return String_List.init(self.source.getContextNames())
    @property
    def has_context_names(self):
        return self.source.hasContextNames()

    @property
    def context_text(self):
        """
        A textual representation of the type/definition of context nodes
        """
        return String_List.init(self.source.getContextText())
    @property
    def has_context_text(self):
        return self.source.hasContextText()

    @property
    def conclusion_text(self):
        """
        A textual representation of the conclusion of the proof state.
        """
        temp = self.source.getConclusionText()
        return (<char*>temp.begin())[:temp.size()]
    @property
    def has_conclusion_text(self):
        return self.source.hasConclusionText()

    @property
    def text(self):
        """
        A textual representation of the proof state.
        """
        temp = self.source.getText()
        return (<char*>temp.begin())[:temp.size()]
    @property
    def has_text(self):
        return self.source.hasText()

    @property
    def id(self):
        """
        A unique identifier of the proof state. Any two proof states in a tactical proof that have an equal id
        can morally be regarded to be 'the same' proof state.
        IMPORTANT: Two proof states with the same id may still have different contents. This is because proof states
                   can contain existential variables (represented by the `evar` node) that can be filled as a
                   side-effect by a tactic running on another proof state.
        """
        return self.source.getId()
pytact.graph_api_capnp.ProofState.schema = _Schema()._init(getProofState_Schema()).as_struct()

cdef class Argument_Reader:
    """
    A concrete argument of a tactic.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asArgument()

    @staticmethod
    cdef init(C_Argument_Reader source):
        cdef Argument_Reader wrapper = Argument_Reader.__new__(Argument_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)
    @property
    def which(self):
        return Argument_Which(self.source.which())
    @property
    def which_raw(self):
        return self.source.which()

    @property
    def unresolvable(self):
        """
        An argument that is currently unresolvable due to limitations of the extraction process.
        """
        return self.source.getUnresolvable()
    @property
    def is_unresolvable(self):
        return self.source.isUnresolvable()

    @property
    def term(self):
        """
        The root of a graph representing an argument that is a term in the calculus of constructions.
        """
        return Argument_Term_Reader.init(self.source.getTerm())
    @property
    def is_term(self):
        return self.source.isTerm()
pytact.graph_api_capnp.Argument.schema = _Schema()._init(getArgument_Schema()).as_struct()

cdef class ProofState_Root_Reader:
    """
    The entry-point of the proof state, all nodes that are 'part of' the proof state are reachable from here.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asProofState_Root()

    @staticmethod
    cdef init(C_ProofState_Root_Reader source):
        cdef ProofState_Root_Reader wrapper = ProofState_Root_Reader.__new__(ProofState_Root_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def dep_index(self):
        return self.source.getDepIndex()

    @property
    def node_index(self):
        return self.source.getNodeIndex()

cdef class Node_Reader:

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asNode()

    @staticmethod
    cdef init(C_Node_Reader source):
        cdef Node_Reader wrapper = Node_Reader.__new__(Node_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def dep_index(self):
        return self.source.getDepIndex()

    @property
    def node_index(self):
        return self.source.getNodeIndex()
pytact.graph_api_capnp.Node.schema = _Schema()._init(getNode_Schema()).as_struct()

cdef class Argument_Term_Reader:
    """
    The root of a graph representing an argument that is a term in the calculus of constructions.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asArgument_Term()

    @staticmethod
    cdef init(C_Argument_Term_Reader source):
        cdef Argument_Term_Reader wrapper = Argument_Term_Reader.__new__(Argument_Term_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def dep_index(self):
        return self.source.getDepIndex()

    @property
    def node_index(self):
        return self.source.getNodeIndex()

cdef class Graph_EdgeTarget_Target_Reader:

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asGraph_EdgeTarget_Target()

    @staticmethod
    cdef init(C_Graph_EdgeTarget_Target_Reader source):
        cdef Graph_EdgeTarget_Target_Reader wrapper = Graph_EdgeTarget_Target_Reader.__new__(Graph_EdgeTarget_Target_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def dep_index(self):
        """
        Indicates to which graph a node belongs. How this should be resolved is not specified here.
        However, a value of zero always points to the 'current' graph.
        """
        return self.source.getDepIndex()

    @property
    def node_index(self):
        """
        The index into `Graph.nodes` where this node can be found.
        """
        return self.source.getNodeIndex()

cdef class AbstractTactic_Reader:

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asAbstractTactic()

    @staticmethod
    cdef init(C_AbstractTactic_Reader source):
        cdef AbstractTactic_Reader wrapper = AbstractTactic_Reader.__new__(AbstractTactic_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def ident(self):
        """
        An abstract tactic is referenced to using a identifier (hash).
        """
        return self.source.getIdent()

    @property
    def parameters(self):
        """
        Every tactic has a constant number of parameters that need to be filled in.
        """
        return self.source.getParameters()
pytact.graph_api_capnp.AbstractTactic.schema = _Schema()._init(getAbstractTactic_Schema()).as_struct()

cdef class Dataset_Reader:
    """
    Every file in the dataset contains a single message of type `Dataset`. Every file corresponds to
    a Coq source file, and contains a representation of all definitions that have existed at any point
    throughout the compilation of the source file.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asDataset()

    @staticmethod
    cdef init(C_Dataset_Reader source):
        cdef Dataset_Reader wrapper = Dataset_Reader.__new__(Dataset_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def dependencies(self):
        """
        The graph contained in a file may reference nodes from the graph of other files. This field maps
        a `DepIndex` into the a file that contains that particular node.
        The first file in this list is always the current file.
        """
        return String_List.init(self.source.getDependencies())
    @property
    def has_dependencies(self):
        return self.source.hasDependencies()

    @property
    def graph(self):
        return Graph_Reader.init(self.source.getGraph())
    @property
    def has_graph(self):
        return self.source.hasGraph()

    @property
    def representative(self):
        """
        The entry point of the global context of definitions that are available when this file is 'Required' by
        another file. The full global context can be obtained by following the `previous` node of definitions.
        If the compilation unit does not contain any 'super'-global definitions this is set to `len(graph.nodes)`
        """
        return self.source.getRepresentative()

    @property
    def definitions(self):
        """
        All of the definitions present in the graph.
        Note that some of these nodes may not be part of the 'super-global' context that is reachable using the
        `representative` field as an entry point. The reason is that the global context is a forest (list of tree's)
        and the 'super-global' context is only the main spine of this forest.
        """
        return Uint32_List.init(self.source.getDefinitions())
    @property
    def has_definitions(self):
        return self.source.hasDefinitions()

    @property
    def module_name(self):
        """
        The name of the module defined by this file.
        """
        temp = self.source.getModuleName()
        return (<char*>temp.begin())[:temp.size()]
    @property
    def has_module_name(self):
        return self.source.hasModuleName()
pytact.graph_api_capnp.Dataset.schema = _Schema()._init(getDataset_Schema()).as_struct()

cdef class Exception_Reader:
    """
    A list of things that can go wrong during reinforcement learning.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asException()

    @staticmethod
    cdef init(C_Exception_Reader source):
        cdef Exception_Reader wrapper = Exception_Reader.__new__(Exception_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)
    @property
    def which(self):
        return Exception_Which(self.source.which())
    @property
    def which_raw(self):
        return self.source.which()

    @property
    def no_such_tactic(self):
        return self.source.getNoSuchTactic()
    @property
    def is_no_such_tactic(self):
        return self.source.isNoSuchTactic()

    @property
    def mismatched_arguments(self):
        return self.source.getMismatchedArguments()
    @property
    def is_mismatched_arguments(self):
        return self.source.isMismatchedArguments()

    @property
    def parse_error(self):
        return self.source.getParseError()
    @property
    def is_parse_error(self):
        return self.source.isParseError()

    @property
    def illegal_argument(self):
        return self.source.getIllegalArgument()
    @property
    def is_illegal_argument(self):
        return self.source.isIllegalArgument()
pytact.graph_api_capnp.Exception.schema = _Schema()._init(getException_Schema()).as_struct()

cdef class ExecutionResult_Reader:
    """
    The result of executing a tactic on a proof state.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asExecutionResult()

    @staticmethod
    cdef init(C_ExecutionResult_Reader source):
        cdef ExecutionResult_Reader wrapper = ExecutionResult_Reader.__new__(ExecutionResult_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)
    @property
    def which(self):
        return ExecutionResult_Which(self.source.which())
    @property
    def which_raw(self):
        return self.source.which()

    @property
    def failure(self):
        """
        The tactic execution failed. This is not an error condition, but rather the natural failure of the tactic.
        """
        return self.source.getFailure()
    @property
    def is_failure(self):
        return self.source.isFailure()

    @property
    def complete(self):
        """
        The proof has been completed.
        """
        return self.source.getComplete()
    @property
    def is_complete(self):
        return self.source.isComplete()

    @property
    def new_state(self):
        """
        The tactic ran successfully and produced a new proof state.
        """
        return ExecutionResult_NewState_Reader.init(self.source.getNewState())
    @property
    def is_new_state(self):
        return self.source.isNewState()

    @property
    def protocol_error(self):
        """
        Indicates a programmer error.
        """
        return Exception_Reader.init(self.source.getProtocolError())
    @property
    def is_protocol_error(self):
        return self.source.isProtocolError()
    @property
    def has_protocol_error(self):
        return self.source.hasProtocolError()
pytact.graph_api_capnp.ExecutionResult.schema = _Schema()._init(getExecutionResult_Schema()).as_struct()

cdef class ExecutionResult_NewState_Reader:
    """
    The tactic ran successfully and produced a new proof state.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asExecutionResult_NewState()

    @staticmethod
    cdef init(C_ExecutionResult_NewState_Reader source):
        cdef ExecutionResult_NewState_Reader wrapper = ExecutionResult_NewState_Reader.__new__(ExecutionResult_NewState_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def graph(self):
        return Graph_Reader.init(self.source.getGraph())
    @property
    def has_graph(self):
        return self.source.hasGraph()

    @property
    def state(self):
        return ProofState_Reader.init(self.source.getState())
    @property
    def has_state(self):
        return self.source.hasState()

cdef class PredictionProtocol_Reader:
    """
    This protocol works by exchanging raw messages over a socket. The protocol is fully linear.
    Coq sends a `Request` and waits for a corresponding `Response` message. A message of a given
    type should be responded with using the obviously corresponding response type.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asPredictionProtocol()

    @staticmethod
    cdef init(C_PredictionProtocol_Reader source):
        cdef PredictionProtocol_Reader wrapper = PredictionProtocol_Reader.__new__(PredictionProtocol_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)
pytact.graph_api_capnp.PredictionProtocol.schema = _Schema()._init(getPredictionProtocol_Schema()).as_struct()

cdef class PredictionProtocol_Request_Reader:

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asPredictionProtocol_Request()

    @staticmethod
    cdef init(C_PredictionProtocol_Request_Reader source):
        cdef PredictionProtocol_Request_Reader wrapper = PredictionProtocol_Request_Reader.__new__(PredictionProtocol_Request_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)
    @property
    def which(self):
        return PredictionProtocol_Request_Which(self.source.which())
    @property
    def which_raw(self):
        return self.source.which()

    @property
    def initialize(self):
        """
        Start a context for making tactical predictions for proof search. The context includes the tactics
        that are currently available, the definitions that are available.
        """
        return PredictionProtocol_Request_Initialize_Reader.init(self.source.getInitialize())
    @property
    def is_initialize(self):
        return self.source.isInitialize()

    @property
    def predict(self):
        """
        Request a list of tactic predictions given the graph of a proof state.
        """
        return PredictionProtocol_Request_Predict_Reader.init(self.source.getPredict())
    @property
    def is_predict(self):
        return self.source.isPredict()

    @property
    def synchronize(self):
        """
        Coq uses this message to synchronize the state of the protocol when exceptions have occurred.
        The contract is that the given integer needs to be echo'd back verbatim.
        """
        return self.source.getSynchronize()
    @property
    def is_synchronize(self):
        return self.source.isSynchronize()

    @property
    def check_alignment(self):
        """
        Request for the server to align the given tactics and definition to it's internal knowledge
        and report back any tactics and definitions that were not found
        """
        return PredictionProtocol_Request_CheckAlignment_Reader.init(self.source.getCheckAlignment())
    @property
    def is_check_alignment(self):
        return self.source.isCheckAlignment()
pytact.graph_api_capnp.PredictionProtocol.Request.schema = _Schema()._init(getPredictionProtocol_Request_Schema()).as_struct()

cdef class PredictionProtocol_Request_Initialize_Reader:
    """
    Start a context for making tactical predictions for proof search. The context includes the tactics
    that are currently available, the definitions that are available.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asPredictionProtocol_Request_Initialize()

    @staticmethod
    cdef init(C_PredictionProtocol_Request_Initialize_Reader source):
        cdef PredictionProtocol_Request_Initialize_Reader wrapper = PredictionProtocol_Request_Initialize_Reader.__new__(PredictionProtocol_Request_Initialize_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def tactics(self):
        return AbstractTactic_Reader_List.init(self.source.getTactics())
    @property
    def has_tactics(self):
        return self.source.hasTactics()

    @property
    def graph(self):
        return Graph_Reader.init(self.source.getGraph())
    @property
    def has_graph(self):
        return self.source.hasGraph()

    @property
    def definitions(self):
        return Uint32_List.init(self.source.getDefinitions())
    @property
    def has_definitions(self):
        return self.source.hasDefinitions()

    @property
    def log_annotation(self):
        temp = self.source.getLogAnnotation()
        return (<char*>temp.begin())[:temp.size()]
    @property
    def has_log_annotation(self):
        return self.source.hasLogAnnotation()

cdef class PredictionProtocol_Request_Predict_Reader:
    """
    Request a list of tactic predictions given the graph of a proof state.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asPredictionProtocol_Request_Predict()

    @staticmethod
    cdef init(C_PredictionProtocol_Request_Predict_Reader source):
        cdef PredictionProtocol_Request_Predict_Reader wrapper = PredictionProtocol_Request_Predict_Reader.__new__(PredictionProtocol_Request_Predict_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def graph(self):
        """
        The graph may reference definitions that were transmitted during the the `initialize` message.
        This graph is designated using the `DepIndex` 1.
        """
        return Graph_Reader.init(self.source.getGraph())
    @property
    def has_graph(self):
        return self.source.hasGraph()

    @property
    def state(self):
        return ProofState_Reader.init(self.source.getState())
    @property
    def has_state(self):
        return self.source.hasState()

cdef class PredictionProtocol_Request_CheckAlignment_Reader:
    """
    Request for the server to align the given tactics and definition to it's internal knowledge
    and report back any tactics and definitions that were not found
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asPredictionProtocol_Request_CheckAlignment()

    @staticmethod
    cdef init(C_PredictionProtocol_Request_CheckAlignment_Reader source):
        cdef PredictionProtocol_Request_CheckAlignment_Reader wrapper = PredictionProtocol_Request_CheckAlignment_Reader.__new__(PredictionProtocol_Request_CheckAlignment_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def tactics(self):
        return AbstractTactic_Reader_List.init(self.source.getTactics())
    @property
    def has_tactics(self):
        return self.source.hasTactics()

    @property
    def graph(self):
        return Graph_Reader.init(self.source.getGraph())
    @property
    def has_graph(self):
        return self.source.hasGraph()

    @property
    def definitions(self):
        return Uint32_List.init(self.source.getDefinitions())
    @property
    def has_definitions(self):
        return self.source.hasDefinitions()

cdef class PredictionProtocol_Prediction_Reader:

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asPredictionProtocol_Prediction()

    @staticmethod
    cdef init(C_PredictionProtocol_Prediction_Reader source):
        cdef PredictionProtocol_Prediction_Reader wrapper = PredictionProtocol_Prediction_Reader.__new__(PredictionProtocol_Prediction_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def tactic(self):
        return Tactic_Reader.init(self.source.getTactic())
    @property
    def has_tactic(self):
        return self.source.hasTactic()

    @property
    def arguments(self):
        return Argument_Reader_List.init(self.source.getArguments())
    @property
    def has_arguments(self):
        return self.source.hasArguments()

    @property
    def confidence(self):
        return self.source.getConfidence()
pytact.graph_api_capnp.PredictionProtocol.Prediction.schema = _Schema()._init(getPredictionProtocol_Prediction_Schema()).as_struct()

cdef class PredictionProtocol_TextPrediction_Reader:

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asPredictionProtocol_TextPrediction()

    @staticmethod
    cdef init(C_PredictionProtocol_TextPrediction_Reader source):
        cdef PredictionProtocol_TextPrediction_Reader wrapper = PredictionProtocol_TextPrediction_Reader.__new__(PredictionProtocol_TextPrediction_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def tactic_text(self):
        temp = self.source.getTacticText()
        return (<char*>temp.begin())[:temp.size()]
    @property
    def has_tactic_text(self):
        return self.source.hasTacticText()

    @property
    def confidence(self):
        return self.source.getConfidence()
pytact.graph_api_capnp.PredictionProtocol.TextPrediction.schema = _Schema()._init(getPredictionProtocol_TextPrediction_Schema()).as_struct()

cdef class PredictionProtocol_Response_Reader:
    """
    See Request for documentation.
    """

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asPredictionProtocol_Response()

    @staticmethod
    cdef init(C_PredictionProtocol_Response_Reader source):
        cdef PredictionProtocol_Response_Reader wrapper = PredictionProtocol_Response_Reader.__new__(PredictionProtocol_Response_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)
    @property
    def which(self):
        return PredictionProtocol_Response_Which(self.source.which())
    @property
    def which_raw(self):
        return self.source.which()

    @property
    def initialized(self):
        return self.source.getInitialized()
    @property
    def is_initialized(self):
        return self.source.isInitialized()

    @property
    def prediction(self):
        return PredictionProtocol_Prediction_Reader_List.init(self.source.getPrediction())
    @property
    def is_prediction(self):
        return self.source.isPrediction()
    @property
    def has_prediction(self):
        return self.source.hasPrediction()

    @property
    def text_prediction(self):
        """
        Output is a list of predictions with a confidence. The list is expected to be
        sorted by decreasing confidence.
        """
        return PredictionProtocol_TextPrediction_Reader_List.init(self.source.getTextPrediction())
    @property
    def is_text_prediction(self):
        return self.source.isTextPrediction()
    @property
    def has_text_prediction(self):
        return self.source.hasTextPrediction()

    @property
    def synchronized(self):
        return self.source.getSynchronized()
    @property
    def is_synchronized(self):
        return self.source.isSynchronized()

    @property
    def alignment(self):
        return PredictionProtocol_Response_Alignment_Reader.init(self.source.getAlignment())
    @property
    def is_alignment(self):
        return self.source.isAlignment()
pytact.graph_api_capnp.PredictionProtocol.Response.schema = _Schema()._init(getPredictionProtocol_Response_Schema()).as_struct()

cdef class PredictionProtocol_Response_Alignment_Reader:

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asPredictionProtocol_Response_Alignment()

    @staticmethod
    cdef init(C_PredictionProtocol_Response_Alignment_Reader source):
        cdef PredictionProtocol_Response_Alignment_Reader wrapper = PredictionProtocol_Response_Alignment_Reader.__new__(PredictionProtocol_Response_Alignment_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)

    @property
    def unaligned_tactics(self):
        return Uint64_List.init(self.source.getUnalignedTactics())
    @property
    def has_unaligned_tactics(self):
        return self.source.hasUnalignedTactics()

    @property
    def unaligned_definitions(self):
        return Uint32_List.init(self.source.getUnalignedDefinitions())
    @property
    def has_unaligned_definitions(self):
        return self.source.hasUnalignedDefinitions()

cdef class ConflatableEdges_Reader:

    def __init__(self, _DynamicStructReader dyn):
        self.source = (<C_DynamicStruct_Reader>dyn.thisptr).asConflatableEdges()

    @staticmethod
    cdef init(C_ConflatableEdges_Reader source):
        cdef ConflatableEdges_Reader wrapper = ConflatableEdges_Reader.__new__(ConflatableEdges_Reader)
        wrapper.source = source
        return wrapper

    @property
    def dynamic(self):
        return to_python_reader(<DynamicValue.Reader>self.source, None)
    def __repr__(self):
        return repr(self.dynamic)
pytact.graph_api_capnp.ConflatableEdges.schema = _Schema()._init(getConflatableEdges_Schema()).as_struct()


cdef class Uint16_List:

    @staticmethod
    cdef init(C_Uint16_List source):
        cdef Uint16_List wrapper = Uint16_List.__new__(Uint16_List)
        wrapper.source = source
        return wrapper

    def __getitem__(self, uint index):
        source = self.source
        if index >= source.size():
            raise IndexError('Out of bounds')
        return source[index]

    def __len__(self):
        return self.source.size()

cdef class Uint32_List:

    @staticmethod
    cdef init(C_Uint32_List source):
        cdef Uint32_List wrapper = Uint32_List.__new__(Uint32_List)
        wrapper.source = source
        return wrapper

    def __getitem__(self, uint index):
        source = self.source
        if index >= source.size():
            raise IndexError('Out of bounds')
        return source[index]

    def __len__(self):
        return self.source.size()

cdef class Uint64_List:

    @staticmethod
    cdef init(C_Uint64_List source):
        cdef Uint64_List wrapper = Uint64_List.__new__(Uint64_List)
        wrapper.source = source
        return wrapper

    def __getitem__(self, uint index):
        source = self.source
        if index >= source.size():
            raise IndexError('Out of bounds')
        return source[index]

    def __len__(self):
        return self.source.size()

cdef class String_List:

    @staticmethod
    cdef init(C_String_List source):
        cdef String_List wrapper = String_List.__new__(String_List)
        wrapper.source = source
        return wrapper

    def __getitem__(self, uint index):
        source = self.source
        if index >= source.size():
            raise IndexError('Out of bounds')
        temp = source[index]
        return (<char*>temp.begin())[:temp.size()]

    def __len__(self):
        return self.source.size()
cdef class PredictionProtocol_TextPrediction_Reader_List:

    @staticmethod
    cdef init(C_PredictionProtocol_TextPrediction_Reader_List source):
        cdef PredictionProtocol_TextPrediction_Reader_List wrapper = PredictionProtocol_TextPrediction_Reader_List.__new__(PredictionProtocol_TextPrediction_Reader_List)
        wrapper.source = source
        return wrapper

    def __getitem__(self, uint index):
        source = self.source
        if index >= source.size():
            raise IndexError('Out of bounds')
        return PredictionProtocol_TextPrediction_Reader.init(source[index])

    def __len__(self):
        return self.source.size()
cdef class ProofState_Reader_List:

    @staticmethod
    cdef init(C_ProofState_Reader_List source):
        cdef ProofState_Reader_List wrapper = ProofState_Reader_List.__new__(ProofState_Reader_List)
        wrapper.source = source
        return wrapper

    def __getitem__(self, uint index):
        source = self.source
        if index >= source.size():
            raise IndexError('Out of bounds')
        return ProofState_Reader.init(source[index])

    def __len__(self):
        return self.source.size()
cdef class Outcome_Reader_List:

    @staticmethod
    cdef init(C_Outcome_Reader_List source):
        cdef Outcome_Reader_List wrapper = Outcome_Reader_List.__new__(Outcome_Reader_List)
        wrapper.source = source
        return wrapper

    def __getitem__(self, uint index):
        source = self.source
        if index >= source.size():
            raise IndexError('Out of bounds')
        return Outcome_Reader.init(source[index])

    def __len__(self):
        return self.source.size()
cdef class PredictionProtocol_Prediction_Reader_List:

    @staticmethod
    cdef init(C_PredictionProtocol_Prediction_Reader_List source):
        cdef PredictionProtocol_Prediction_Reader_List wrapper = PredictionProtocol_Prediction_Reader_List.__new__(PredictionProtocol_Prediction_Reader_List)
        wrapper.source = source
        return wrapper

    def __getitem__(self, uint index):
        source = self.source
        if index >= source.size():
            raise IndexError('Out of bounds')
        return PredictionProtocol_Prediction_Reader.init(source[index])

    def __len__(self):
        return self.source.size()
cdef class Graph_Node_Reader_List:

    @staticmethod
    cdef init(C_Graph_Node_Reader_List source):
        cdef Graph_Node_Reader_List wrapper = Graph_Node_Reader_List.__new__(Graph_Node_Reader_List)
        wrapper.source = source
        return wrapper

    def __getitem__(self, uint index):
        source = self.source
        if index >= source.size():
            raise IndexError('Out of bounds')
        return Graph_Node_Reader.init(source[index])

    def __len__(self):
        return self.source.size()
cdef class AbstractTactic_Reader_List:

    @staticmethod
    cdef init(C_AbstractTactic_Reader_List source):
        cdef AbstractTactic_Reader_List wrapper = AbstractTactic_Reader_List.__new__(AbstractTactic_Reader_List)
        wrapper.source = source
        return wrapper

    def __getitem__(self, uint index):
        source = self.source
        if index >= source.size():
            raise IndexError('Out of bounds')
        return AbstractTactic_Reader.init(source[index])

    def __len__(self):
        return self.source.size()
cdef class Node_Reader_List:

    @staticmethod
    cdef init(C_Node_Reader_List source):
        cdef Node_Reader_List wrapper = Node_Reader_List.__new__(Node_Reader_List)
        wrapper.source = source
        return wrapper

    def __getitem__(self, uint index):
        source = self.source
        if index >= source.size():
            raise IndexError('Out of bounds')
        return Node_Reader.init(source[index])

    def __len__(self):
        return self.source.size()
cdef class Argument_Reader_List:

    @staticmethod
    cdef init(C_Argument_Reader_List source):
        cdef Argument_Reader_List wrapper = Argument_Reader_List.__new__(Argument_Reader_List)
        wrapper.source = source
        return wrapper

    def __getitem__(self, uint index):
        source = self.source
        if index >= source.size():
            raise IndexError('Out of bounds')
        return Argument_Reader.init(source[index])

    def __len__(self):
        return self.source.size()
cdef class ProofStep_Reader_List:

    @staticmethod
    cdef init(C_ProofStep_Reader_List source):
        cdef ProofStep_Reader_List wrapper = ProofStep_Reader_List.__new__(ProofStep_Reader_List)
        wrapper.source = source
        return wrapper

    def __getitem__(self, uint index):
        source = self.source
        if index >= source.size():
            raise IndexError('Out of bounds')
        return ProofStep_Reader.init(source[index])

    def __len__(self):
        return self.source.size()
cdef class Graph_EdgeTarget_Reader_List:

    @staticmethod
    cdef init(C_Graph_EdgeTarget_Reader_List source):
        cdef Graph_EdgeTarget_Reader_List wrapper = Graph_EdgeTarget_Reader_List.__new__(Graph_EdgeTarget_Reader_List)
        wrapper.source = source
        return wrapper

    def __getitem__(self, uint index):
        source = self.source
        if index >= source.size():
            raise IndexError('Out of bounds')
        return Graph_EdgeTarget_Reader.init(source[index])

    def __len__(self):
        return self.source.size()