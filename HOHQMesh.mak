#
##################################################
# Modify the following for your local installation
##################################################
#
F90 = /usr/local/bin/gfortran
HOQMeshPath = /Users/davidkopriva/Programming/My\ Code/Fortran/HOHQMesh
FTOLPath = /Users/davidkopriva/Programming/My\ Code/Fortran/FTObjectLibrary

FFLAGS = -cpp -O
##########################
# Object Files for build #
##########################

OBJS = \
3DMeshController.o \
BoundaryEdgeCleaning.o \
ChainedSegmentedCurveClass.o \
CommandLineReader.o \
Connections.o \
ControlFileReader.o \
CurveConversions.o \
CurveInterpolantClass.o \
ElementOperations.o \
Encoder.o \
EquationEvaluatorClass.o \
FatalErrorException.o \
FileAndStringProcessing.o \
fmin.o \
Frenet.o \
FRSegmentedCurveClass.o \
FTDataClass.o \
FTDictionaryClass.o \
FTExceptionClass.o \
FTLinkedListClass.o \
FTMultiIndexTable.o \
FTObjectArrayClass.o \
FTObjectClass.o \
FTOLConstants.o \
FTSparseMatrixClass.o \
FTStackClass.o \
FTStringSetClass.o \
FTValueClass.o \
FTValueDictionaryClass.o \
Geometry.o \
Geometry3D.o \
Hash.o \
HexMeshObjects.o \
HOHQMeshMain.o \
InterfaceElementMethods.o \
InterpolationAndDerivatives.o \
LaplaceMeshSmoother.o \
Mesh3DOutputMethods.o \
MeshBoundaryMethods.o \
MeshCleaner.o \
MeshGeneratorMethods.o \
MeshOperationsModule.o \
MeshOutputMethods.o \
MeshProject.o \
MeshQualityAnalysis.o \
MeshSmoother.o \
Misc.o \
NodesTemplates.o \
ObjectArrayAdditions.o \
ParametricEquationCurveClass.o \
ProgramGlobals.o \
QuadTreeGridClass.o \
QuadTreeGridGenerator.o \
QuadTreeTemplateOperations.o \
ReaderExceptions.o \
SegmentedCurveArrayClass.o \
Shortcuts.o \
SimpleSweep.o \
Sizer.o \
SizerControls.o \
SMChainedCurveClass.o \
SMCircularArc.o \
SMConstants.o \
SMCurveClass.o \
SMLine.o \
SMMeshClass.o \
SMMeshObjects.o \
SMModel.o \
SMSplineCurveClass.o \
SpringMeshSmoother.o \
SweeperClass.o \
Templates.o \
TimerClass.o \
TransfiniteMapClass.o \
Utilities.o \

HOHQMesh : $(OBJS)
	 ${F90}  -o $@ $(OBJS)

#######################################
# Object dependencies and compilation #
#######################################
3DMeshController.o : ${HOQMeshPath}/Source/3DSource/3DMeshController.f90 \
FTValueDictionaryClass.o \
SimpleSweep.o \
SweeperClass.o \
HexMeshObjects.o \
SMMeshObjects.o \
FatalErrorException.o \
MeshProject.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/3DSource/3DMeshController.f90

BoundaryEdgeCleaning.o : ${HOQMeshPath}/Source/Mesh/BoundaryEdgeCleaning.f90 \
Connections.o \
ProgramGlobals.o \
MeshBoundaryMethods.o \
SMModel.o \
SMMeshClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Mesh/BoundaryEdgeCleaning.f90

ChainedSegmentedCurveClass.o : ${HOQMeshPath}/Source/Curves/DiscreteCurves/ChainedSegmentedCurveClass.f90 \
FTExceptionClass.o \
FTValueClass.o \
FTExceptionClass.o \
FRSegmentedCurveClass.o \
FTObjectArrayClass.o \
FTDataClass.o \
SMConstants.o \
ProgramGlobals.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Curves/DiscreteCurves/ChainedSegmentedCurveClass.f90

CommandLineReader.o : ${HOQMeshPath}/Source/Foundation/CommandLineReader.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Foundation/CommandLineReader.f90

Connections.o : ${HOQMeshPath}/Source/Mesh/Connections.f90 \
FTLinkedListClass.o \
SMMeshObjects.o \
MeshOutputMethods.o \
SMMeshClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Mesh/Connections.f90

ControlFileReader.o : ${HOQMeshPath}/Source/IO/ControlFileReader.f90 \
FTExceptionClass.o \
FatalErrorException.o \
FTExceptionClass.o \
FTStringSetClass.o \
Encoder.o \
FTValueDictionaryClass.o \
FTStackClass.o \
FTLinkedListClass.o \
FTDataClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/IO/ControlFileReader.f90

CurveConversions.o : ${HOQMeshPath}/Source/Curves/DiscreteCurves/CurveConversions.f90 \
ChainedSegmentedCurveClass.o \
SizerControls.o \
SegmentedCurveArrayClass.o \
SMChainedCurveClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Curves/DiscreteCurves/CurveConversions.f90

CurveInterpolantClass.o : ${HOQMeshPath}/Source/3DSource/Geometry/CurveInterpolantClass.f90 \
InterpolationAndDerivatives.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/3DSource/Geometry/CurveInterpolantClass.f90

ElementOperations.o : ${HOQMeshPath}/Source/MeshObjects/ElementOperations.f90 \
SMMeshClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/MeshObjects/ElementOperations.f90

Encoder.o : ${HOQMeshPath}/Source/Foundation/Encoder.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Foundation/Encoder.f90

EquationEvaluatorClass.o : ${HOQMeshPath}/Source/Curves/ContinuousCurves/EquationEvaluatorClass.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Curves/ContinuousCurves/EquationEvaluatorClass.f90

FatalErrorException.o : ${HOQMeshPath}/Source/Foundation/FatalErrorException.f90 \
FTExceptionClass.o \
FTValueClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Foundation/FatalErrorException.f90

FileAndStringProcessing.o : ${HOQMeshPath}/Source/IO/FileAndStringProcessing.f90 \
SMConstants.o \
ProgramGlobals.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/IO/FileAndStringProcessing.f90

fmin.o : ${HOQMeshPath}/Source/Curves/fmin.f90 \
SMCurveClass.o \
ProgramGlobals.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Curves/fmin.f90

Frenet.o : ${HOQMeshPath}/Source/3DSource/Geometry/Frenet.f90 \
SMCurveClass.o \
Geometry3D.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/3DSource/Geometry/Frenet.f90

FRSegmentedCurveClass.o : ${HOQMeshPath}/Source/Curves/DiscreteCurves/FRSegmentedCurveClass.f90 \
SMConstants.o \
SMCurveClass.o \
ProgramGlobals.o \
Geometry.o \
FTObjectArrayClass.o \
FTLinkedListClass.o \
SizerControls.o \
ObjectArrayAdditions.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Curves/DiscreteCurves/FRSegmentedCurveClass.f90

FTDataClass.o : ${FTOLPath}/Source/FTObjects/FTDataClass.f90 \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${FTOLPath}/Source/FTObjects/FTDataClass.f90

FTDictionaryClass.o : ${FTOLPath}/Source/FTObjects/FTDictionaryClass.f90 \
FTObjectArrayClass.o \
FTLinkedListClass.o \
FTObjectClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${FTOLPath}/Source/FTObjects/FTDictionaryClass.f90

FTExceptionClass.o : ${FTOLPath}/Source/FTObjects/FTExceptionClass.f90 \
FTDictionaryClass.o \
FTValueDictionaryClass.o \
FTStackClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${FTOLPath}/Source/FTObjects/FTExceptionClass.f90

FTLinkedListClass.o : ${FTOLPath}/Source/FTObjects/FTLinkedListClass.f90 \
FTObjectArrayClass.o \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${FTOLPath}/Source/FTObjects/FTLinkedListClass.f90

FTMultiIndexTable.o : ${FTOLPath}/Source/FTObjects/FTMultiIndexTable.f90 \
FTObjectClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${FTOLPath}/Source/FTObjects/FTMultiIndexTable.f90

FTObjectArrayClass.o : ${FTOLPath}/Source/FTObjects/FTObjectArrayClass.f90 \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${FTOLPath}/Source/FTObjects/FTObjectArrayClass.f90

FTObjectClass.o : ${FTOLPath}/Source/FTObjects/FTObjectClass.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${FTOLPath}/Source/FTObjects/FTObjectClass.f90

FTOLConstants.o : ${FTOLPath}/Source/Foundation/FTOLConstants.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${FTOLPath}/Source/Foundation/FTOLConstants.f90

FTSparseMatrixClass.o : ${FTOLPath}/Source/FTObjects/FTSparseMatrixClass.f90 \
FTLinkedListClass.o \
FTObjectClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${FTOLPath}/Source/FTObjects/FTSparseMatrixClass.f90

FTStackClass.o : ${FTOLPath}/Source/FTObjects/FTStackClass.f90 \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${FTOLPath}/Source/FTObjects/FTStackClass.f90

FTStringSetClass.o : ${FTOLPath}/Source/FTObjects/FTStringSetClass.f90 \
FTObjectClass.o \
FTDictionaryClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${FTOLPath}/Source/FTObjects/FTStringSetClass.f90

FTValueClass.o : ${FTOLPath}/Source/FTObjects/FTValueClass.f90 \
FTOLConstants.o \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${FTOLPath}/Source/FTObjects/FTValueClass.f90

FTValueDictionaryClass.o : ${FTOLPath}/Source/FTObjects/FTValueDictionaryClass.f90 \
FTValueClass.o \
FTDictionaryClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${FTOLPath}/Source/FTObjects/FTValueDictionaryClass.f90

Geometry.o : ${HOQMeshPath}/Source/Foundation/Geometry.f90 \
SMConstants.o \
ProgramGlobals.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Foundation/Geometry.f90

Geometry3D.o : ${HOQMeshPath}/Source/3DSource/Geometry/Geometry3D.f90 \
SMConstants.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/3DSource/Geometry/Geometry3D.f90

Hash.o : ${FTOLPath}/Source/FTObjects/Hash.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${FTOLPath}/Source/FTObjects/Hash.f90

HexMeshObjects.o : ${HOQMeshPath}/Source/3DSource/HexMeshObjects.f90 \
SMConstants.o \
SMMeshObjects.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/3DSource/HexMeshObjects.f90

HOHQMeshMain.o : ${HOQMeshPath}/Source/HOHQMeshMain.f90 \
MeshQualityAnalysis.o \
MeshCleaner.o \
ProgramGlobals.o \
ControlFileReader.o \
Mesh3DOutputMethods.o \
FTValueDictionaryClass.o \
MeshProject.o \
TimerClass.o \
CommandLineReader.o \
MeshOutputMethods.o \
3DMeshController.o \
MeshGeneratorMethods.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/HOHQMeshMain.f90

InterfaceElementMethods.o : ${HOQMeshPath}/Source/Mesh/InterfaceElementMethods.f90 \
MeshProject.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Mesh/InterfaceElementMethods.f90

InterpolationAndDerivatives.o : ${HOQMeshPath}/Source/3DSource/Geometry/InterpolationAndDerivatives.f90 \
SMConstants.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/3DSource/Geometry/InterpolationAndDerivatives.f90

LaplaceMeshSmoother.o : ${HOQMeshPath}/Source/Mesh/LaplaceMeshSmoother.f90 \
Connections.o \
MeshSmoother.o \
SMModel.o \
SMMeshClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Mesh/LaplaceMeshSmoother.f90

Mesh3DOutputMethods.o : ${HOQMeshPath}/Source/3DSource/Mesh3DOutputMethods.f90 \
HexMeshObjects.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/3DSource/Mesh3DOutputMethods.f90

MeshBoundaryMethods.o : ${HOQMeshPath}/Source/Mesh/MeshBoundaryMethods.f90 \
CurveConversions.o \
FatalErrorException.o \
Geometry.o \
Connections.o \
SMChainedCurveClass.o \
ProgramGlobals.o \
SMModel.o \
SMMeshClass.o \
fmin.o \
Sizer.o \
MeshOutputMethods.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Mesh/MeshBoundaryMethods.f90

MeshCleaner.o : ${HOQMeshPath}/Source/Mesh/MeshCleaner.f90 \
MeshQualityAnalysis.o \
InterfaceElementMethods.o \
FatalErrorException.o \
Connections.o \
ElementOperations.o \
SMModel.o \
SMMeshObjects.o \
SMMeshClass.o \
fmin.o \
Geometry.o \
MeshBoundaryMethods.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Mesh/MeshCleaner.f90

MeshGeneratorMethods.o : ${HOQMeshPath}/Source/Mesh/MeshGeneratorMethods.f90 \
FatalErrorException.o \
MeshCleaner.o \
ProgramGlobals.o \
TransfiniteMapClass.o \
Sizer.o \
BoundaryEdgeCleaning.o \
Geometry.o \
QuadTreeGridGenerator.o \
MeshBoundaryMethods.o \
CurveInterpolantClass.o \
MeshOperationsModule.o \
MeshProject.o \
fmin.o \
MeshOutputMethods.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Mesh/MeshGeneratorMethods.f90

MeshOperationsModule.o : ${HOQMeshPath}/Source/Mesh/MeshOperationsModule.f90 \
FTObjectClass.o \
QuadTreeGridClass.o \
ProgramGlobals.o \
FTLinkedListClass.o \
SMMeshObjects.o \
SMMeshClass.o \
FTLinkedListClass.o \
SMConstants.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Mesh/MeshOperationsModule.f90

MeshOutputMethods.o : ${HOQMeshPath}/Source/IO/MeshOutputMethods.f90 \
FTObjectArrayClass.o \
MeshOperationsModule.o \
SMModel.o \
SMMeshObjects.o \
MeshQualityAnalysis.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/IO/MeshOutputMethods.f90

MeshProject.o : ${HOQMeshPath}/Source/Project/MeshProject.f90 \
FTValueDictionaryClass.o \
FatalErrorException.o \
SMModel.o \
MeshSmoother.o \
Shortcuts.o \
HexMeshObjects.o \
SMChainedCurveClass.o \
CurveConversions.o \
Sizer.o \
LaplaceMeshSmoother.o \
QuadTreeGridClass.o \
Geometry3D.o \
SMMeshClass.o \
SpringMeshSmoother.o \
FTExceptionClass.o \
ChainedSegmentedCurveClass.o \
SMConstants.o \
SizerControls.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Project/MeshProject.f90

MeshQualityAnalysis.o : ${HOQMeshPath}/Source/Mesh/MeshQualityAnalysis.f90 \
SMMeshObjects.o \
SMMeshClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Mesh/MeshQualityAnalysis.f90

MeshSmoother.o : ${HOQMeshPath}/Source/Mesh/MeshSmoother.f90 \
MeshBoundaryMethods.o \
SMModel.o \
SMMeshClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Mesh/MeshSmoother.f90

Misc.o : ${HOQMeshPath}/Source/Foundation/Misc.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Foundation/Misc.f90

NodesTemplates.o : ${HOQMeshPath}/Source/QuadTreeGrid/NodesTemplates.f90 \
ProgramGlobals.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/QuadTreeGrid/NodesTemplates.f90

ObjectArrayAdditions.o : ${HOQMeshPath}/Source/Categories/ObjectArrayAdditions.f90 \
FTObjectArrayClass.o \
FTLinkedListClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Categories/ObjectArrayAdditions.f90

ParametricEquationCurveClass.o : ${HOQMeshPath}/Source/Curves/ContinuousCurves/ParametricEquationCurveClass.f90 \
FTExceptionClass.o \
FTValueClass.o \
SMConstants.o \
EquationEvaluatorClass.o \
SMCurveClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Curves/ContinuousCurves/ParametricEquationCurveClass.f90

ProgramGlobals.o : ${HOQMeshPath}/Source/Foundation/ProgramGlobals.f90 \
SMConstants.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Foundation/ProgramGlobals.f90

QuadTreeGridClass.o : ${HOQMeshPath}/Source/QuadTreeGrid/QuadTreeGridClass.f90 \
Sizer.o \
SMConstants.o \
SMMeshObjects.o \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/QuadTreeGrid/QuadTreeGridClass.f90

QuadTreeGridGenerator.o : ${HOQMeshPath}/Source/QuadTreeGrid/QuadTreeGridGenerator.f90 \
Sizer.o \
QuadTreeGridClass.o \
QuadTreeTemplateOperations.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/QuadTreeGrid/QuadTreeGridGenerator.f90

QuadTreeTemplateOperations.o : ${HOQMeshPath}/Source/QuadTreeGrid/QuadTreeTemplateOperations.f90 \
QuadTreeGridClass.o \
SMConstants.o \
SMMeshObjects.o \
Templates.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/QuadTreeGrid/QuadTreeTemplateOperations.f90

ReaderExceptions.o : ${HOQMeshPath}/Source/3DSource/ReaderExceptions.f90 \
FTExceptionClass.o \
FTValueDictionaryClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/3DSource/ReaderExceptions.f90

SegmentedCurveArrayClass.o : ${HOQMeshPath}/Source/Curves/DiscreteCurves/SegmentedCurveArrayClass.f90 \
Geometry.o \
ProgramGlobals.o \
SMConstants.o \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Curves/DiscreteCurves/SegmentedCurveArrayClass.f90

Shortcuts.o : ${HOQMeshPath}/Source/Foundation/Shortcuts.f90 \
FTExceptionClass.o \
FTValueDictionaryClass.o \
FatalErrorException.o \
SMConstants.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Foundation/Shortcuts.f90

SimpleSweep.o : ${HOQMeshPath}/Source/3DSource/SimpleSweep.f90 \
FTExceptionClass.o \
HexMeshObjects.o \
FTExceptionClass.o \
FatalErrorException.o \
FTObjectArrayClass.o \
FTValueDictionaryClass.o \
MeshProject.o \
SMMeshClass.o \
SMConstants.o \
ProgramGlobals.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/3DSource/SimpleSweep.f90

Sizer.o : ${HOQMeshPath}/Source/Project/Sizer/Sizer.f90 \
SizerControls.o \
ChainedSegmentedCurveClass.o \
ProgramGlobals.o \
SMConstants.o \
Geometry.o \
FTLinkedListClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Project/Sizer/Sizer.f90

SizerControls.o : ${HOQMeshPath}/Source/Project/Sizer/SizerControls.f90 \
FTLinkedListClass.o \
SMConstants.o \
FTObjectClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Project/Sizer/SizerControls.f90

SMChainedCurveClass.o : ${HOQMeshPath}/Source/Curves/ContinuousCurves/SMChainedCurveClass.f90 \
ProgramGlobals.o \
SMCurveClass.o \
FTExceptionClass.o \
FTValueClass.o \
FTObjectArrayClass.o \
FTLinkedListClass.o \
FTExceptionClass.o \
FTObjectClass.o \
FTLinkedListClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Curves/ContinuousCurves/SMChainedCurveClass.f90

SMCircularArc.o : ${HOQMeshPath}/Source/Curves/ContinuousCurves/SMCircularArc.f90 \
SMCurveClass.o \
SMConstants.o \
ProgramGlobals.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Curves/ContinuousCurves/SMCircularArc.f90

SMConstants.o : ${HOQMeshPath}/Source/Foundation/SMConstants.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Foundation/SMConstants.f90

SMCurveClass.o : ${HOQMeshPath}/Source/Curves/ContinuousCurves/SMCurveClass.f90 \
ProgramGlobals.o \
SMConstants.o \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Curves/ContinuousCurves/SMCurveClass.f90

SMLine.o : ${HOQMeshPath}/Source/Curves/ContinuousCurves/SMLine.f90 \
SMCurveClass.o \
SMConstants.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Curves/ContinuousCurves/SMLine.f90

SMMeshClass.o : ${HOQMeshPath}/Source/Project/Mesh/SMMeshClass.f90 \
SegmentedCurveArrayClass.o \
FTObjectArrayClass.o \
FTLinkedListClass.o \
FTSparseMatrixClass.o \
SMMeshObjects.o \
FTObjectClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Project/Mesh/SMMeshClass.f90

SMMeshObjects.o : ${HOQMeshPath}/Source/MeshObjects/SMMeshObjects.f90 \
FTObjectArrayClass.o \
ProgramGlobals.o \
SMConstants.o \
FTLinkedListClass.o \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/MeshObjects/SMMeshObjects.f90

SMModel.o : ${HOQMeshPath}/Source/Project/Model/SMModel.f90 \
SMLine.o \
FTValueDictionaryClass.o \
FatalErrorException.o \
FTValueClass.o \
Shortcuts.o \
FTExceptionClass.o \
SweeperClass.o \
ProgramGlobals.o \
SMSplineCurveClass.o \
FTLinkedListClass.o \
Encoder.o \
SMCircularArc.o \
FTExceptionClass.o \
FTDataClass.o \
SMChainedCurveClass.o \
ParametricEquationCurveClass.o \
SMConstants.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Project/Model/SMModel.f90

SMSplineCurveClass.o : ${HOQMeshPath}/Source/Curves/ContinuousCurves/SMSplineCurveClass.f90 \
SMCurveClass.o \
SMConstants.o \
Geometry.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Curves/ContinuousCurves/SMSplineCurveClass.f90

SpringMeshSmoother.o : ${HOQMeshPath}/Source/Mesh/SpringMeshSmoother.f90 \
FatalErrorException.o \
MeshSmoother.o \
MeshBoundaryMethods.o \
SMModel.o \
FTValueDictionaryClass.o \
SMMeshClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Mesh/SpringMeshSmoother.f90

SweeperClass.o : ${HOQMeshPath}/Source/3DSource/SweeperClass.f90 \
FTExceptionClass.o \
HexMeshObjects.o \
FTExceptionClass.o \
FatalErrorException.o \
SMChainedCurveClass.o \
FTValueDictionaryClass.o \
Geometry3D.o \
Frenet.o \
SMConstants.o \
ProgramGlobals.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/3DSource/SweeperClass.f90

Templates.o : ${HOQMeshPath}/Source/QuadTreeGrid/Templates.f90 \
QuadTreeGridClass.o \
SMConstants.o \
SMMeshObjects.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/QuadTreeGrid/Templates.f90

TimerClass.o : ${HOQMeshPath}/Source/Foundation/TimerClass.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/Foundation/TimerClass.f90

TransfiniteMapClass.o : ${HOQMeshPath}/Source/3DSource/Geometry/TransfiniteMapClass.f90 \
CurveInterpolantClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/3DSource/Geometry/TransfiniteMapClass.f90

Utilities.o : ${HOQMeshPath}/Source/3DSource/Geometry/Utilities.f90 \
SMConstants.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${HOQMeshPath}/Source/3DSource/Geometry/Utilities.f90

