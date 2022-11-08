from capnp.includes.types cimport *
from capnp cimport helpers
from capnp.includes.capnp_cpp cimport DynamicValue, Schema, StringPtr
from capnp.lib.capnp cimport _DynamicStructReader, to_python_reader, _Schema

from capnp.helpers.non_circular cimport reraise_kj_exception

cdef extern from "capnp/helpers/capabilityHelper.cpp":
    pass

cdef extern from "graph_api.capnp.h":

    Schema getGraph_Node_Schema"capnp::Schema::from<Graph::Node>"()
    cdef cppclass C_Graph_Node_Reader "Graph::Node::Reader":
        C_Graph_Node_Label_Reader getLabel() except +reraise_kj_exception
        uint32_t getChildrenIndex() except +reraise_kj_exception
        uint16_t getChildrenCount() except +reraise_kj_exception
        int64_t getIdentity() except +reraise_kj_exception

    Schema getGraph_EdgeTarget_Schema"capnp::Schema::from<Graph::EdgeTarget>"()
    cdef cppclass C_Graph_EdgeTarget_Reader "Graph::EdgeTarget::Reader":
        EdgeClassification getLabel() except +reraise_kj_exception
        C_Graph_EdgeTarget_Target_Reader getTarget() except +reraise_kj_exception

    Schema getGraph_Schema"capnp::Schema::from<Graph>"()
    cdef cppclass C_Graph_Reader "Graph::Reader":
        C_Graph_Node_Reader_List getNodes() except +reraise_kj_exception
        cbool hasNodes() except +reraise_kj_exception
        C_Graph_EdgeTarget_Reader_List getEdges() except +reraise_kj_exception
        cbool hasEdges() except +reraise_kj_exception

    Schema getDefinition_Schema"capnp::Schema::from<Definition>"()
    cdef cppclass C_Definition_Reader "Definition::Reader":
        Definition_Which which() except +reraise_kj_exception
        StringPtr getName() except +reraise_kj_exception
        cbool hasName() except +reraise_kj_exception
        uint32_t getPrevious() except +reraise_kj_exception
        C_Uint16_List getExternalPrevious() except +reraise_kj_exception
        cbool hasExternalPrevious() except +reraise_kj_exception
        C_Definition_Status_Reader getStatus() except +reraise_kj_exception
        uint32_t getInductive() except +reraise_kj_exception
        cbool isInductive() except +reraise_kj_exception
        uint32_t getConstructor() except +reraise_kj_exception
        cbool isConstructor() except +reraise_kj_exception
        uint32_t getProjection() except +reraise_kj_exception
        cbool isProjection() except +reraise_kj_exception
        void getManualConstant() except +reraise_kj_exception
        cbool isManualConstant() except +reraise_kj_exception
        C_ProofStep_Reader_List getTacticalConstant() except +reraise_kj_exception
        cbool isTacticalConstant() except +reraise_kj_exception
        cbool hasTacticalConstant() except +reraise_kj_exception
        void getManualSectionConstant() except +reraise_kj_exception
        cbool isManualSectionConstant() except +reraise_kj_exception
        C_ProofStep_Reader_List getTacticalSectionConstant() except +reraise_kj_exception
        cbool isTacticalSectionConstant() except +reraise_kj_exception
        cbool hasTacticalSectionConstant() except +reraise_kj_exception
        StringPtr getTypeText() except +reraise_kj_exception
        cbool hasTypeText() except +reraise_kj_exception
        StringPtr getTermText() except +reraise_kj_exception
        cbool hasTermText() except +reraise_kj_exception
    cpdef enum class Definition_Which "Definition::Which"(uint16_t):
        INDUCTIVE,
        CONSTRUCTOR,
        PROJECTION,
        MANUAL_CONSTANT,
        TACTICAL_CONSTANT,
        MANUAL_SECTION_CONSTANT,
        TACTICAL_SECTION_CONSTANT,

    Schema getIntP_Schema"capnp::Schema::from<IntP>"()
    cdef cppclass C_IntP_Reader "IntP::Reader":
        uint64_t getValue() except +reraise_kj_exception

    Schema getFloatP_Schema"capnp::Schema::from<FloatP>"()
    cdef cppclass C_FloatP_Reader "FloatP::Reader":
        double getValue() except +reraise_kj_exception

    Schema getGraph_Node_Label_Schema"capnp::Schema::from<Graph::Node::Label>"()
    cdef cppclass C_Graph_Node_Label_Reader "Graph::Node::Label::Reader":
        Graph_Node_Label_Which which() except +reraise_kj_exception
        void getProofState() except +reraise_kj_exception
        cbool isProofState() except +reraise_kj_exception
        void getContextDef() except +reraise_kj_exception
        cbool isContextDef() except +reraise_kj_exception
        void getContextAssum() except +reraise_kj_exception
        cbool isContextAssum() except +reraise_kj_exception
        C_Definition_Reader getDefinition() except +reraise_kj_exception
        cbool isDefinition() except +reraise_kj_exception
        cbool hasDefinition() except +reraise_kj_exception
        void getConstEmpty() except +reraise_kj_exception
        cbool isConstEmpty() except +reraise_kj_exception
        void getSortSProp() except +reraise_kj_exception
        cbool isSortSProp() except +reraise_kj_exception
        void getSortProp() except +reraise_kj_exception
        cbool isSortProp() except +reraise_kj_exception
        void getSortSet() except +reraise_kj_exception
        cbool isSortSet() except +reraise_kj_exception
        void getSortType() except +reraise_kj_exception
        cbool isSortType() except +reraise_kj_exception
        void getRel() except +reraise_kj_exception
        cbool isRel() except +reraise_kj_exception
        uint32_t getEvar() except +reraise_kj_exception
        cbool isEvar() except +reraise_kj_exception
        void getEvarSubst() except +reraise_kj_exception
        cbool isEvarSubst() except +reraise_kj_exception
        void getCast() except +reraise_kj_exception
        cbool isCast() except +reraise_kj_exception
        void getProd() except +reraise_kj_exception
        cbool isProd() except +reraise_kj_exception
        void getLambda() except +reraise_kj_exception
        cbool isLambda() except +reraise_kj_exception
        void getLetIn() except +reraise_kj_exception
        cbool isLetIn() except +reraise_kj_exception
        void getApp() except +reraise_kj_exception
        cbool isApp() except +reraise_kj_exception
        void getCase() except +reraise_kj_exception
        cbool isCase() except +reraise_kj_exception
        void getCaseBranch() except +reraise_kj_exception
        cbool isCaseBranch() except +reraise_kj_exception
        void getFix() except +reraise_kj_exception
        cbool isFix() except +reraise_kj_exception
        void getFixFun() except +reraise_kj_exception
        cbool isFixFun() except +reraise_kj_exception
        void getCoFix() except +reraise_kj_exception
        cbool isCoFix() except +reraise_kj_exception
        void getCoFixFun() except +reraise_kj_exception
        cbool isCoFixFun() except +reraise_kj_exception
        C_IntP_Reader getInt() except +reraise_kj_exception
        cbool isInt() except +reraise_kj_exception
        cbool hasInt() except +reraise_kj_exception
        C_FloatP_Reader getFloat() except +reraise_kj_exception
        cbool isFloat() except +reraise_kj_exception
        cbool hasFloat() except +reraise_kj_exception
        StringPtr getPrimitive() except +reraise_kj_exception
        cbool isPrimitive() except +reraise_kj_exception
        cbool hasPrimitive() except +reraise_kj_exception
        void getUndefProofState() except +reraise_kj_exception
        cbool isUndefProofState() except +reraise_kj_exception
    cpdef enum class Graph_Node_Label_Which "Graph::Node::Label::Which"(uint16_t):
        PROOF_STATE,
        CONTEXT_DEF,
        CONTEXT_ASSUM,
        DEFINITION,
        CONST_EMPTY,
        SORT_S_PROP,
        SORT_PROP,
        SORT_SET,
        SORT_TYPE,
        REL,
        EVAR,
        EVAR_SUBST,
        CAST,
        PROD,
        LAMBDA,
        LET_IN,
        APP,
        CASE,
        CASE_BRANCH,
        FIX,
        FIX_FUN,
        CO_FIX,
        CO_FIX_FUN,
        INT,
        FLOAT,
        PRIMITIVE,
        UNDEF_PROOF_STATE,

    Schema getDefinition_Status_Substituted_Schema"capnp::Schema::from<Definition::Status::Substituted>"()
    cdef cppclass C_Definition_Status_Substituted_Reader "Definition::Status::Substituted::Reader":
        uint16_t getDepIndex() except +reraise_kj_exception
        uint32_t getNodeIndex() except +reraise_kj_exception

    Schema getDefinition_Status_Schema"capnp::Schema::from<Definition::Status>"()
    cdef cppclass C_Definition_Status_Reader "Definition::Status::Reader":
        Definition_Status_Which which() except +reraise_kj_exception
        void getOriginal() except +reraise_kj_exception
        cbool isOriginal() except +reraise_kj_exception
        uint32_t getDischarged() except +reraise_kj_exception
        cbool isDischarged() except +reraise_kj_exception
        C_Definition_Status_Substituted_Reader getSubstituted() except +reraise_kj_exception
        cbool isSubstituted() except +reraise_kj_exception
    cpdef enum class Definition_Status_Which "Definition::Status::Which"(uint16_t):
        ORIGINAL,
        DISCHARGED,
        SUBSTITUTED,

    Schema getProofStep_Schema"capnp::Schema::from<ProofStep>"()
    cdef cppclass C_ProofStep_Reader "ProofStep::Reader":
        C_ProofStep_Tactic_Reader getTactic() except +reraise_kj_exception
        C_Outcome_Reader_List getOutcomes() except +reraise_kj_exception
        cbool hasOutcomes() except +reraise_kj_exception

    Schema getTactic_Schema"capnp::Schema::from<Tactic>"()
    cdef cppclass C_Tactic_Reader "Tactic::Reader":
        uint64_t getIdent() except +reraise_kj_exception
        StringPtr getText() except +reraise_kj_exception
        cbool hasText() except +reraise_kj_exception
        StringPtr getBaseText() except +reraise_kj_exception
        cbool hasBaseText() except +reraise_kj_exception
        StringPtr getIntermText() except +reraise_kj_exception
        cbool hasIntermText() except +reraise_kj_exception
        cbool getExact() except +reraise_kj_exception

    Schema getProofStep_Tactic_Schema"capnp::Schema::from<ProofStep::Tactic>"()
    cdef cppclass C_ProofStep_Tactic_Reader "ProofStep::Tactic::Reader":
        ProofStep_Tactic_Which which() except +reraise_kj_exception
        void getUnknown() except +reraise_kj_exception
        cbool isUnknown() except +reraise_kj_exception
        C_Tactic_Reader getKnown() except +reraise_kj_exception
        cbool isKnown() except +reraise_kj_exception
        cbool hasKnown() except +reraise_kj_exception
    cpdef enum class ProofStep_Tactic_Which "ProofStep::Tactic::Which"(uint16_t):
        UNKNOWN,
        KNOWN,

    Schema getOutcome_Schema"capnp::Schema::from<Outcome>"()
    cdef cppclass C_Outcome_Reader "Outcome::Reader":
        C_ProofState_Reader getBefore() except +reraise_kj_exception
        cbool hasBefore() except +reraise_kj_exception
        C_ProofState_Reader_List getAfter() except +reraise_kj_exception
        cbool hasAfter() except +reraise_kj_exception
        C_Outcome_Term_Reader getTerm() except +reraise_kj_exception
        StringPtr getTermText() except +reraise_kj_exception
        cbool hasTermText() except +reraise_kj_exception
        C_Argument_Reader_List getTacticArguments() except +reraise_kj_exception
        cbool hasTacticArguments() except +reraise_kj_exception

    Schema getOutcome_Term_Schema"capnp::Schema::from<Outcome::Term>"()
    cdef cppclass C_Outcome_Term_Reader "Outcome::Term::Reader":
        uint16_t getDepIndex() except +reraise_kj_exception
        uint32_t getNodeIndex() except +reraise_kj_exception

    Schema getProofState_Schema"capnp::Schema::from<ProofState>"()
    cdef cppclass C_ProofState_Reader "ProofState::Reader":
        C_ProofState_Root_Reader getRoot() except +reraise_kj_exception
        C_Node_Reader_List getContext() except +reraise_kj_exception
        cbool hasContext() except +reraise_kj_exception
        C_String_List getContextNames() except +reraise_kj_exception
        cbool hasContextNames() except +reraise_kj_exception
        C_String_List getContextText() except +reraise_kj_exception
        cbool hasContextText() except +reraise_kj_exception
        StringPtr getConclusionText() except +reraise_kj_exception
        cbool hasConclusionText() except +reraise_kj_exception
        StringPtr getText() except +reraise_kj_exception
        cbool hasText() except +reraise_kj_exception
        uint32_t getId() except +reraise_kj_exception

    Schema getArgument_Schema"capnp::Schema::from<Argument>"()
    cdef cppclass C_Argument_Reader "Argument::Reader":
        Argument_Which which() except +reraise_kj_exception
        void getUnresolvable() except +reraise_kj_exception
        cbool isUnresolvable() except +reraise_kj_exception
        C_Argument_Term_Reader getTerm() except +reraise_kj_exception
        cbool isTerm() except +reraise_kj_exception
    cpdef enum class Argument_Which "Argument::Which"(uint16_t):
        UNRESOLVABLE,
        TERM,

    Schema getProofState_Root_Schema"capnp::Schema::from<ProofState::Root>"()
    cdef cppclass C_ProofState_Root_Reader "ProofState::Root::Reader":
        uint16_t getDepIndex() except +reraise_kj_exception
        uint32_t getNodeIndex() except +reraise_kj_exception

    Schema getNode_Schema"capnp::Schema::from<Node>"()
    cdef cppclass C_Node_Reader "Node::Reader":
        uint16_t getDepIndex() except +reraise_kj_exception
        uint32_t getNodeIndex() except +reraise_kj_exception

    Schema getArgument_Term_Schema"capnp::Schema::from<Argument::Term>"()
    cdef cppclass C_Argument_Term_Reader "Argument::Term::Reader":
        uint16_t getDepIndex() except +reraise_kj_exception
        uint32_t getNodeIndex() except +reraise_kj_exception

    Schema getGraph_EdgeTarget_Target_Schema"capnp::Schema::from<Graph::EdgeTarget::Target>"()
    cdef cppclass C_Graph_EdgeTarget_Target_Reader "Graph::EdgeTarget::Target::Reader":
        uint16_t getDepIndex() except +reraise_kj_exception
        uint32_t getNodeIndex() except +reraise_kj_exception

    Schema getAbstractTactic_Schema"capnp::Schema::from<AbstractTactic>"()
    cdef cppclass C_AbstractTactic_Reader "AbstractTactic::Reader":
        uint64_t getIdent() except +reraise_kj_exception
        uint8_t getParameters() except +reraise_kj_exception

    Schema getDataset_Schema"capnp::Schema::from<Dataset>"()
    cdef cppclass C_Dataset_Reader "Dataset::Reader":
        C_String_List getDependencies() except +reraise_kj_exception
        cbool hasDependencies() except +reraise_kj_exception
        C_Graph_Reader getGraph() except +reraise_kj_exception
        cbool hasGraph() except +reraise_kj_exception
        uint32_t getRepresentative() except +reraise_kj_exception
        C_Uint32_List getDefinitions() except +reraise_kj_exception
        cbool hasDefinitions() except +reraise_kj_exception
        StringPtr getModuleName() except +reraise_kj_exception
        cbool hasModuleName() except +reraise_kj_exception

    Schema getException_Schema"capnp::Schema::from<Exception>"()
    cdef cppclass C_Exception_Reader "Exception::Reader":
        Exception_Which which() except +reraise_kj_exception
        void getNoSuchTactic() except +reraise_kj_exception
        cbool isNoSuchTactic() except +reraise_kj_exception
        void getMismatchedArguments() except +reraise_kj_exception
        cbool isMismatchedArguments() except +reraise_kj_exception
        void getParseError() except +reraise_kj_exception
        cbool isParseError() except +reraise_kj_exception
        void getIllegalArgument() except +reraise_kj_exception
        cbool isIllegalArgument() except +reraise_kj_exception
    cpdef enum class Exception_Which "Exception::Which"(uint16_t):
        NO_SUCH_TACTIC,
        MISMATCHED_ARGUMENTS,
        PARSE_ERROR,
        ILLEGAL_ARGUMENT,

    Schema getExecutionResult_Schema"capnp::Schema::from<ExecutionResult>"()
    cdef cppclass C_ExecutionResult_Reader "ExecutionResult::Reader":
        ExecutionResult_Which which() except +reraise_kj_exception
        void getFailure() except +reraise_kj_exception
        cbool isFailure() except +reraise_kj_exception
        void getComplete() except +reraise_kj_exception
        cbool isComplete() except +reraise_kj_exception
        C_ExecutionResult_NewState_Reader getNewState() except +reraise_kj_exception
        cbool isNewState() except +reraise_kj_exception
        C_Exception_Reader getProtocolError() except +reraise_kj_exception
        cbool isProtocolError() except +reraise_kj_exception
        cbool hasProtocolError() except +reraise_kj_exception
    cpdef enum class ExecutionResult_Which "ExecutionResult::Which"(uint16_t):
        FAILURE,
        COMPLETE,
        NEW_STATE,
        PROTOCOL_ERROR,

    Schema getExecutionResult_NewState_Schema"capnp::Schema::from<ExecutionResult::NewState>"()
    cdef cppclass C_ExecutionResult_NewState_Reader "ExecutionResult::NewState::Reader":
        C_Graph_Reader getGraph() except +reraise_kj_exception
        cbool hasGraph() except +reraise_kj_exception
        C_ProofState_Reader getState() except +reraise_kj_exception
        cbool hasState() except +reraise_kj_exception

    Schema getPredictionProtocol_Schema"capnp::Schema::from<PredictionProtocol>"()
    cdef cppclass C_PredictionProtocol_Reader "PredictionProtocol::Reader":
        pass

    Schema getPredictionProtocol_Request_Schema"capnp::Schema::from<PredictionProtocol::Request>"()
    cdef cppclass C_PredictionProtocol_Request_Reader "PredictionProtocol::Request::Reader":
        PredictionProtocol_Request_Which which() except +reraise_kj_exception
        C_PredictionProtocol_Request_Initialize_Reader getInitialize() except +reraise_kj_exception
        cbool isInitialize() except +reraise_kj_exception
        C_PredictionProtocol_Request_Predict_Reader getPredict() except +reraise_kj_exception
        cbool isPredict() except +reraise_kj_exception
        uint64_t getSynchronize() except +reraise_kj_exception
        cbool isSynchronize() except +reraise_kj_exception
        C_PredictionProtocol_Request_CheckAlignment_Reader getCheckAlignment() except +reraise_kj_exception
        cbool isCheckAlignment() except +reraise_kj_exception
    cpdef enum class PredictionProtocol_Request_Which "PredictionProtocol::Request::Which"(uint16_t):
        INITIALIZE,
        PREDICT,
        SYNCHRONIZE,
        CHECK_ALIGNMENT,

    Schema getPredictionProtocol_Request_Initialize_Schema"capnp::Schema::from<PredictionProtocol::Request::Initialize>"()
    cdef cppclass C_PredictionProtocol_Request_Initialize_Reader "PredictionProtocol::Request::Initialize::Reader":
        C_AbstractTactic_Reader_List getTactics() except +reraise_kj_exception
        cbool hasTactics() except +reraise_kj_exception
        C_Graph_Reader getGraph() except +reraise_kj_exception
        cbool hasGraph() except +reraise_kj_exception
        C_Uint32_List getDefinitions() except +reraise_kj_exception
        cbool hasDefinitions() except +reraise_kj_exception
        StringPtr getLogAnnotation() except +reraise_kj_exception
        cbool hasLogAnnotation() except +reraise_kj_exception

    Schema getPredictionProtocol_Request_Predict_Schema"capnp::Schema::from<PredictionProtocol::Request::Predict>"()
    cdef cppclass C_PredictionProtocol_Request_Predict_Reader "PredictionProtocol::Request::Predict::Reader":
        C_Graph_Reader getGraph() except +reraise_kj_exception
        cbool hasGraph() except +reraise_kj_exception
        C_ProofState_Reader getState() except +reraise_kj_exception
        cbool hasState() except +reraise_kj_exception

    Schema getPredictionProtocol_Request_CheckAlignment_Schema"capnp::Schema::from<PredictionProtocol::Request::CheckAlignment>"()
    cdef cppclass C_PredictionProtocol_Request_CheckAlignment_Reader "PredictionProtocol::Request::CheckAlignment::Reader":
        C_AbstractTactic_Reader_List getTactics() except +reraise_kj_exception
        cbool hasTactics() except +reraise_kj_exception
        C_Graph_Reader getGraph() except +reraise_kj_exception
        cbool hasGraph() except +reraise_kj_exception
        C_Uint32_List getDefinitions() except +reraise_kj_exception
        cbool hasDefinitions() except +reraise_kj_exception

    Schema getPredictionProtocol_Prediction_Schema"capnp::Schema::from<PredictionProtocol::Prediction>"()
    cdef cppclass C_PredictionProtocol_Prediction_Reader "PredictionProtocol::Prediction::Reader":
        C_Tactic_Reader getTactic() except +reraise_kj_exception
        cbool hasTactic() except +reraise_kj_exception
        C_Argument_Reader_List getArguments() except +reraise_kj_exception
        cbool hasArguments() except +reraise_kj_exception
        double getConfidence() except +reraise_kj_exception

    Schema getPredictionProtocol_TextPrediction_Schema"capnp::Schema::from<PredictionProtocol::TextPrediction>"()
    cdef cppclass C_PredictionProtocol_TextPrediction_Reader "PredictionProtocol::TextPrediction::Reader":
        StringPtr getTacticText() except +reraise_kj_exception
        cbool hasTacticText() except +reraise_kj_exception
        double getConfidence() except +reraise_kj_exception

    Schema getPredictionProtocol_Response_Schema"capnp::Schema::from<PredictionProtocol::Response>"()
    cdef cppclass C_PredictionProtocol_Response_Reader "PredictionProtocol::Response::Reader":
        PredictionProtocol_Response_Which which() except +reraise_kj_exception
        void getInitialized() except +reraise_kj_exception
        cbool isInitialized() except +reraise_kj_exception
        C_PredictionProtocol_Prediction_Reader_List getPrediction() except +reraise_kj_exception
        cbool isPrediction() except +reraise_kj_exception
        cbool hasPrediction() except +reraise_kj_exception
        C_PredictionProtocol_TextPrediction_Reader_List getTextPrediction() except +reraise_kj_exception
        cbool isTextPrediction() except +reraise_kj_exception
        cbool hasTextPrediction() except +reraise_kj_exception
        uint64_t getSynchronized() except +reraise_kj_exception
        cbool isSynchronized() except +reraise_kj_exception
        C_PredictionProtocol_Response_Alignment_Reader getAlignment() except +reraise_kj_exception
        cbool isAlignment() except +reraise_kj_exception
    cpdef enum class PredictionProtocol_Response_Which "PredictionProtocol::Response::Which"(uint16_t):
        INITIALIZED,
        PREDICTION,
        TEXT_PREDICTION,
        SYNCHRONIZED,
        ALIGNMENT,

    Schema getPredictionProtocol_Response_Alignment_Schema"capnp::Schema::from<PredictionProtocol::Response::Alignment>"()
    cdef cppclass C_PredictionProtocol_Response_Alignment_Reader "PredictionProtocol::Response::Alignment::Reader":
        C_Uint64_List getUnalignedTactics() except +reraise_kj_exception
        cbool hasUnalignedTactics() except +reraise_kj_exception
        C_Uint32_List getUnalignedDefinitions() except +reraise_kj_exception
        cbool hasUnalignedDefinitions() except +reraise_kj_exception

    Schema getConflatableEdges_Schema"capnp::Schema::from<ConflatableEdges>"()
    cdef cppclass C_ConflatableEdges_Reader "ConflatableEdges::Reader":
        pass
    cpdef enum class EdgeClassification "capnp::schemas::EdgeClassification_ca20cd9e129678c6"(uint16_t):
        CONTEXT_ELEM,
        CONTEXT_SUBJECT,
        CONTEXT_DEF_TYPE,
        CONTEXT_DEF_TERM,
        CONST_TYPE,
        CONST_UNDEF,
        CONST_DEF,
        CONST_OPAQUE_DEF,
        CONST_PRIMITIVE,
        IND_TYPE,
        IND_CONSTRUCT,
        PROJ_TERM,
        CONSTRUCT_TERM,
        CAST_TERM,
        CAST_TYPE,
        PROD_TYPE,
        PROD_TERM,
        LAMBDA_TYPE,
        LAMBDA_TERM,
        LET_IN_DEF,
        LET_IN_TYPE,
        LET_IN_TERM,
        APP_FUN,
        APP_ARG,
        CASE_TERM,
        CASE_RETURN,
        CASE_BRANCH_POINTER,
        CASE_IND,
        C_B_CONSTRUCT,
        C_B_TERM,
        FIX_MUTUAL,
        FIX_RETURN,
        FIX_FUN_TYPE,
        FIX_FUN_TERM,
        CO_FIX_MUTUAL,
        CO_FIX_RETURN,
        CO_FIX_FUN_TYPE,
        CO_FIX_FUN_TERM,
        REL_POINTER,
        EVAR_SUBST_POINTER,
        EVAR_SUBST_TERM,
        EVAR_SUBST_TARGET,
        EVAR_SUBJECT,

    cdef cppclass C_DynamicStruct_Reader" ::capnp::DynamicStruct::Reader":
        C_Graph_Node_Reader asGraph_Node"as<Graph::Node>"()
        C_Graph_EdgeTarget_Reader asGraph_EdgeTarget"as<Graph::EdgeTarget>"()
        C_Graph_Reader asGraph"as<Graph>"()
        C_Definition_Reader asDefinition"as<Definition>"()
        C_IntP_Reader asIntP"as<IntP>"()
        C_FloatP_Reader asFloatP"as<FloatP>"()
        C_Graph_Node_Label_Reader asGraph_Node_Label"as<Graph::Node::Label>"()
        C_Definition_Status_Substituted_Reader asDefinition_Status_Substituted"as<Definition::Status::Substituted>"()
        C_Definition_Status_Reader asDefinition_Status"as<Definition::Status>"()
        C_ProofStep_Reader asProofStep"as<ProofStep>"()
        C_Tactic_Reader asTactic"as<Tactic>"()
        C_ProofStep_Tactic_Reader asProofStep_Tactic"as<ProofStep::Tactic>"()
        C_Outcome_Reader asOutcome"as<Outcome>"()
        C_Outcome_Term_Reader asOutcome_Term"as<Outcome::Term>"()
        C_ProofState_Reader asProofState"as<ProofState>"()
        C_Argument_Reader asArgument"as<Argument>"()
        C_ProofState_Root_Reader asProofState_Root"as<ProofState::Root>"()
        C_Node_Reader asNode"as<Node>"()
        C_Argument_Term_Reader asArgument_Term"as<Argument::Term>"()
        C_Graph_EdgeTarget_Target_Reader asGraph_EdgeTarget_Target"as<Graph::EdgeTarget::Target>"()
        C_AbstractTactic_Reader asAbstractTactic"as<AbstractTactic>"()
        C_Dataset_Reader asDataset"as<Dataset>"()
        C_Exception_Reader asException"as<Exception>"()
        C_ExecutionResult_Reader asExecutionResult"as<ExecutionResult>"()
        C_ExecutionResult_NewState_Reader asExecutionResult_NewState"as<ExecutionResult::NewState>"()
        C_PredictionProtocol_Reader asPredictionProtocol"as<PredictionProtocol>"()
        C_PredictionProtocol_Request_Reader asPredictionProtocol_Request"as<PredictionProtocol::Request>"()
        C_PredictionProtocol_Request_Initialize_Reader asPredictionProtocol_Request_Initialize"as<PredictionProtocol::Request::Initialize>"()
        C_PredictionProtocol_Request_Predict_Reader asPredictionProtocol_Request_Predict"as<PredictionProtocol::Request::Predict>"()
        C_PredictionProtocol_Request_CheckAlignment_Reader asPredictionProtocol_Request_CheckAlignment"as<PredictionProtocol::Request::CheckAlignment>"()
        C_PredictionProtocol_Prediction_Reader asPredictionProtocol_Prediction"as<PredictionProtocol::Prediction>"()
        C_PredictionProtocol_TextPrediction_Reader asPredictionProtocol_TextPrediction"as<PredictionProtocol::TextPrediction>"()
        C_PredictionProtocol_Response_Reader asPredictionProtocol_Response"as<PredictionProtocol::Response>"()
        C_PredictionProtocol_Response_Alignment_Reader asPredictionProtocol_Response_Alignment"as<PredictionProtocol::Response::Alignment>"()
        C_ConflatableEdges_Reader asConflatableEdges"as<ConflatableEdges>"()

cdef extern from "capnp/list.h":
    cdef cppclass C_Uint16_List " ::capnp::List<uint16_t, ::capnp::Kind::PRIMITIVE>::Reader":
        uint16_t operator[](uint) except +reraise_kj_exception
        uint size()
    cdef cppclass C_Uint32_List " ::capnp::List<uint32_t, ::capnp::Kind::PRIMITIVE>::Reader":
        uint32_t operator[](uint) except +reraise_kj_exception
        uint size()
    cdef cppclass C_Uint64_List " ::capnp::List<uint64_t, ::capnp::Kind::PRIMITIVE>::Reader":
        uint64_t operator[](uint) except +reraise_kj_exception
        uint size()
    cdef cppclass C_String_List " ::capnp::List<capnp::Text, ::capnp::Kind::BLOB>::Reader":
        StringPtr operator[](uint) except +reraise_kj_exception
        uint size()
    cdef cppclass C_PredictionProtocol_TextPrediction_Reader_List " ::capnp::List<PredictionProtocol::TextPrediction, ::capnp::Kind::STRUCT>::Reader":
        C_PredictionProtocol_TextPrediction_Reader operator[](uint) except +reraise_kj_exception
        uint size()
    cdef cppclass C_ProofState_Reader_List " ::capnp::List<ProofState, ::capnp::Kind::STRUCT>::Reader":
        C_ProofState_Reader operator[](uint) except +reraise_kj_exception
        uint size()
    cdef cppclass C_Outcome_Reader_List " ::capnp::List<Outcome, ::capnp::Kind::STRUCT>::Reader":
        C_Outcome_Reader operator[](uint) except +reraise_kj_exception
        uint size()
    cdef cppclass C_PredictionProtocol_Prediction_Reader_List " ::capnp::List<PredictionProtocol::Prediction, ::capnp::Kind::STRUCT>::Reader":
        C_PredictionProtocol_Prediction_Reader operator[](uint) except +reraise_kj_exception
        uint size()
    cdef cppclass C_Graph_Node_Reader_List " ::capnp::List<Graph::Node, ::capnp::Kind::STRUCT>::Reader":
        C_Graph_Node_Reader operator[](uint) except +reraise_kj_exception
        uint size()
    cdef cppclass C_AbstractTactic_Reader_List " ::capnp::List<AbstractTactic, ::capnp::Kind::STRUCT>::Reader":
        C_AbstractTactic_Reader operator[](uint) except +reraise_kj_exception
        uint size()
    cdef cppclass C_Node_Reader_List " ::capnp::List<Node, ::capnp::Kind::STRUCT>::Reader":
        C_Node_Reader operator[](uint) except +reraise_kj_exception
        uint size()
    cdef cppclass C_Argument_Reader_List " ::capnp::List<Argument, ::capnp::Kind::STRUCT>::Reader":
        C_Argument_Reader operator[](uint) except +reraise_kj_exception
        uint size()
    cdef cppclass C_ProofStep_Reader_List " ::capnp::List<ProofStep, ::capnp::Kind::STRUCT>::Reader":
        C_ProofStep_Reader operator[](uint) except +reraise_kj_exception
        uint size()
    cdef cppclass C_Graph_EdgeTarget_Reader_List " ::capnp::List<Graph::EdgeTarget, ::capnp::Kind::STRUCT>::Reader":
        C_Graph_EdgeTarget_Reader operator[](uint) except +reraise_kj_exception
        uint size()

cdef class Graph_Node_Reader:
    cdef C_Graph_Node_Reader source
    @staticmethod
    cdef init(C_Graph_Node_Reader source)

cdef class Graph_EdgeTarget_Reader:
    cdef C_Graph_EdgeTarget_Reader source
    @staticmethod
    cdef init(C_Graph_EdgeTarget_Reader source)

cdef class Graph_Reader:
    cdef C_Graph_Reader source
    @staticmethod
    cdef init(C_Graph_Reader source)

cdef class Definition_Reader:
    cdef C_Definition_Reader source
    @staticmethod
    cdef init(C_Definition_Reader source)

cdef class IntP_Reader:
    cdef C_IntP_Reader source
    @staticmethod
    cdef init(C_IntP_Reader source)

cdef class FloatP_Reader:
    cdef C_FloatP_Reader source
    @staticmethod
    cdef init(C_FloatP_Reader source)

cdef class Graph_Node_Label_Reader:
    cdef C_Graph_Node_Label_Reader source
    @staticmethod
    cdef init(C_Graph_Node_Label_Reader source)

cdef class Definition_Status_Substituted_Reader:
    cdef C_Definition_Status_Substituted_Reader source
    @staticmethod
    cdef init(C_Definition_Status_Substituted_Reader source)

cdef class Definition_Status_Reader:
    cdef C_Definition_Status_Reader source
    @staticmethod
    cdef init(C_Definition_Status_Reader source)

cdef class ProofStep_Reader:
    cdef C_ProofStep_Reader source
    @staticmethod
    cdef init(C_ProofStep_Reader source)

cdef class Tactic_Reader:
    cdef C_Tactic_Reader source
    @staticmethod
    cdef init(C_Tactic_Reader source)

cdef class ProofStep_Tactic_Reader:
    cdef C_ProofStep_Tactic_Reader source
    @staticmethod
    cdef init(C_ProofStep_Tactic_Reader source)

cdef class Outcome_Reader:
    cdef C_Outcome_Reader source
    @staticmethod
    cdef init(C_Outcome_Reader source)

cdef class Outcome_Term_Reader:
    cdef C_Outcome_Term_Reader source
    @staticmethod
    cdef init(C_Outcome_Term_Reader source)

cdef class ProofState_Reader:
    cdef C_ProofState_Reader source
    @staticmethod
    cdef init(C_ProofState_Reader source)

cdef class Argument_Reader:
    cdef C_Argument_Reader source
    @staticmethod
    cdef init(C_Argument_Reader source)

cdef class ProofState_Root_Reader:
    cdef C_ProofState_Root_Reader source
    @staticmethod
    cdef init(C_ProofState_Root_Reader source)

cdef class Node_Reader:
    cdef C_Node_Reader source
    @staticmethod
    cdef init(C_Node_Reader source)

cdef class Argument_Term_Reader:
    cdef C_Argument_Term_Reader source
    @staticmethod
    cdef init(C_Argument_Term_Reader source)

cdef class Graph_EdgeTarget_Target_Reader:
    cdef C_Graph_EdgeTarget_Target_Reader source
    @staticmethod
    cdef init(C_Graph_EdgeTarget_Target_Reader source)

cdef class AbstractTactic_Reader:
    cdef C_AbstractTactic_Reader source
    @staticmethod
    cdef init(C_AbstractTactic_Reader source)

cdef class Dataset_Reader:
    cdef C_Dataset_Reader source
    @staticmethod
    cdef init(C_Dataset_Reader source)

cdef class Exception_Reader:
    cdef C_Exception_Reader source
    @staticmethod
    cdef init(C_Exception_Reader source)

cdef class ExecutionResult_Reader:
    cdef C_ExecutionResult_Reader source
    @staticmethod
    cdef init(C_ExecutionResult_Reader source)

cdef class ExecutionResult_NewState_Reader:
    cdef C_ExecutionResult_NewState_Reader source
    @staticmethod
    cdef init(C_ExecutionResult_NewState_Reader source)

cdef class PredictionProtocol_Reader:
    cdef C_PredictionProtocol_Reader source
    @staticmethod
    cdef init(C_PredictionProtocol_Reader source)

cdef class PredictionProtocol_Request_Reader:
    cdef C_PredictionProtocol_Request_Reader source
    @staticmethod
    cdef init(C_PredictionProtocol_Request_Reader source)

cdef class PredictionProtocol_Request_Initialize_Reader:
    cdef C_PredictionProtocol_Request_Initialize_Reader source
    @staticmethod
    cdef init(C_PredictionProtocol_Request_Initialize_Reader source)

cdef class PredictionProtocol_Request_Predict_Reader:
    cdef C_PredictionProtocol_Request_Predict_Reader source
    @staticmethod
    cdef init(C_PredictionProtocol_Request_Predict_Reader source)

cdef class PredictionProtocol_Request_CheckAlignment_Reader:
    cdef C_PredictionProtocol_Request_CheckAlignment_Reader source
    @staticmethod
    cdef init(C_PredictionProtocol_Request_CheckAlignment_Reader source)

cdef class PredictionProtocol_Prediction_Reader:
    cdef C_PredictionProtocol_Prediction_Reader source
    @staticmethod
    cdef init(C_PredictionProtocol_Prediction_Reader source)

cdef class PredictionProtocol_TextPrediction_Reader:
    cdef C_PredictionProtocol_TextPrediction_Reader source
    @staticmethod
    cdef init(C_PredictionProtocol_TextPrediction_Reader source)

cdef class PredictionProtocol_Response_Reader:
    cdef C_PredictionProtocol_Response_Reader source
    @staticmethod
    cdef init(C_PredictionProtocol_Response_Reader source)

cdef class PredictionProtocol_Response_Alignment_Reader:
    cdef C_PredictionProtocol_Response_Alignment_Reader source
    @staticmethod
    cdef init(C_PredictionProtocol_Response_Alignment_Reader source)

cdef class ConflatableEdges_Reader:
    cdef C_ConflatableEdges_Reader source
    @staticmethod
    cdef init(C_ConflatableEdges_Reader source)

cdef class Uint16_List:
    cdef C_Uint16_List source
    @staticmethod
    cdef init(C_Uint16_List source)

cdef class Uint32_List:
    cdef C_Uint32_List source
    @staticmethod
    cdef init(C_Uint32_List source)

cdef class Uint64_List:
    cdef C_Uint64_List source
    @staticmethod
    cdef init(C_Uint64_List source)

cdef class String_List:
    cdef C_String_List source
    @staticmethod
    cdef init(C_String_List source)
cdef class PredictionProtocol_TextPrediction_Reader_List:
    cdef C_PredictionProtocol_TextPrediction_Reader_List source
    @staticmethod
    cdef init(C_PredictionProtocol_TextPrediction_Reader_List source)
cdef class ProofState_Reader_List:
    cdef C_ProofState_Reader_List source
    @staticmethod
    cdef init(C_ProofState_Reader_List source)
cdef class Outcome_Reader_List:
    cdef C_Outcome_Reader_List source
    @staticmethod
    cdef init(C_Outcome_Reader_List source)
cdef class PredictionProtocol_Prediction_Reader_List:
    cdef C_PredictionProtocol_Prediction_Reader_List source
    @staticmethod
    cdef init(C_PredictionProtocol_Prediction_Reader_List source)
cdef class Graph_Node_Reader_List:
    cdef C_Graph_Node_Reader_List source
    @staticmethod
    cdef init(C_Graph_Node_Reader_List source)
cdef class AbstractTactic_Reader_List:
    cdef C_AbstractTactic_Reader_List source
    @staticmethod
    cdef init(C_AbstractTactic_Reader_List source)
cdef class Node_Reader_List:
    cdef C_Node_Reader_List source
    @staticmethod
    cdef init(C_Node_Reader_List source)
cdef class Argument_Reader_List:
    cdef C_Argument_Reader_List source
    @staticmethod
    cdef init(C_Argument_Reader_List source)
cdef class ProofStep_Reader_List:
    cdef C_ProofStep_Reader_List source
    @staticmethod
    cdef init(C_ProofStep_Reader_List source)
cdef class Graph_EdgeTarget_Reader_List:
    cdef C_Graph_EdgeTarget_Reader_List source
    @staticmethod
    cdef init(C_Graph_EdgeTarget_Reader_List source)