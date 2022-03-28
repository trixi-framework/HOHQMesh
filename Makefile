#
##################################################
# Modify the following for your local installation
##################################################
#
FC = gfortran
HOHQMESHPATH = .
FTOLPATH = $(HOHQMESHPATH)/Contrib/FTObjectLibrary

FFLAGS = -cpp -O
##########################
# Object Files for build #
##########################

OBJS = \
3DMeshController.o \
Assert.o \
BiCubicClass.o \
BoundaryEdgeCleaning.o \
ChainedSegmentedCurveClass.o \
CommandLineReader.o \
Comparisons.o \
Connections.o \
ControlFileReader.o \
CurveConversions.o \
CurveInterpolantClass.o \
CurveTests.o \
DataFileTopographyClass.o \
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
GaussianCurvature.o \
Geometry.o \
Geometry3D.o \
Hash.o \
HexMeshObjects.o \
HOHQMeshMain.o \
HOHQMesh.o \
InterfaceElementMethods.o \
InterpolationAndDerivatives.o \
LaplaceMeshSmoother.o \
Mesh3DOutputMethods.o \
MeshBoundaryMethods.o \
MeshCleaner.o \
MeshGeneratorMethods.o \
MeshingTests.o \
MeshOperationsModule.o \
MeshOutputMethods.o \
MeshProject.o \
MeshQualityAnalysis.o \
MeshSmoother.o \
Misc.o \
NodesTemplates.o \
ObjectArrayAdditions.o \
ParametricEquationCurveClass.o \
ParametricEquationTopographyClass.o \
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
SMTopographyClass.o \
SpringMeshSmoother.o \
SweeperClass.o \
Templates.o \
TestDataClass.o \
TestSuiteManagerClass.o \
TimerClass.o \
TransfiniteMapClass.o \
Utilities.o \

HOHQMesh : $(OBJS)
	 ${FC}  -o $@ $(OBJS) $(LDFLAGS)

#######################################
# Object dependencies and compilation #
#######################################
3DMeshController.o: $(HOHQMESHPATH)/Source/3DSource/3DMeshController.f90 \
FTValueDictionaryClass.o \
SimpleSweep.o \
SweeperClass.o \
HexMeshObjects.o \
SMMeshObjects.o \
FatalErrorException.o \
MeshProject.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/3DSource/3DMeshController.f90

Assert.o : $(FTOLPATH)/Source/FTTesting/Assert.f90 \
Comparisons.o \
FTOLConstants.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTTesting/Assert.f90

BiCubicClass.o : $(HOHQMESHPATH)/Source/Surfaces/BiCubicClass.f90 \
TestSuiteManagerClass.o \
FTExceptionClass.o \
SMConstants.o \
Assert.o \
FTObjectClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Surfaces/BiCubicClass.f90

BoundaryEdgeCleaning.o :$(HOHQMESHPATH)/Source/Mesh/BoundaryEdgeCleaning.f90 \
Connections.o \
ProgramGlobals.o \
MeshBoundaryMethods.o \
SMModel.o \
SMMeshClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Mesh/BoundaryEdgeCleaning.f90

ChainedSegmentedCurveClass.o :$(HOHQMESHPATH)/Source/Curves/DiscreteCurves/ChainedSegmentedCurveClass.f90 \
FTExceptionClass.o \
FTValueClass.o \
FTExceptionClass.o \
FRSegmentedCurveClass.o \
FTObjectArrayClass.o \
FTDataClass.o \
SMConstants.o \
ProgramGlobals.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Curves/DiscreteCurves/ChainedSegmentedCurveClass.f90

CommandLineReader.o :$(HOHQMESHPATH)/Source/Foundation/CommandLineReader.f90
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Foundation/CommandLineReader.f90

Comparisons.o : $(FTOLPATH)/Source/FTTesting/Comparisons.f90 \
FTOLConstants.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTTesting/Comparisons.f90

Connections.o :$(HOHQMESHPATH)/Source/Mesh/Connections.f90 \
FTLinkedListClass.o \
SMMeshObjects.o \
MeshOutputMethods.o \
SMMeshClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Mesh/Connections.f90

ControlFileReader.o :$(HOHQMESHPATH)/Source/IO/ControlFileReader.f90 \
FTExceptionClass.o \
FatalErrorException.o \
FTExceptionClass.o \
FTStringSetClass.o \
Encoder.o \
FTValueDictionaryClass.o \
FTStackClass.o \
FTLinkedListClass.o \
FTDataClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/IO/ControlFileReader.f90

CurveConversions.o :$(HOHQMESHPATH)/Source/Curves/DiscreteCurves/CurveConversions.f90 \
ChainedSegmentedCurveClass.o \
SizerControls.o \
SegmentedCurveArrayClass.o \
SMChainedCurveClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Curves/DiscreteCurves/CurveConversions.f90

CurveInterpolantClass.o :$(HOHQMESHPATH)/Source/3DSource/Geometry/CurveInterpolantClass.f90 \
InterpolationAndDerivatives.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/3DSource/Geometry/CurveInterpolantClass.f90

CurveTests.o : $(HOHQMESHPATH)/Source/Testing/CurveTests.f90 \
SMConstants.o \
SMCurveClass.o \
SMCircularArc.o \
FTExceptionClass.o \
SMLine.o \
ParametricEquationCurveClass.o \
TestSuiteManagerClass.o \
Assert.o \
SMSplineCurveClass.o \
SMTopographyClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Testing/CurveTests.f90

DataFileTopographyClass.o : $(HOHQMESHPATH)/Source/Surfaces/DataFileTopographyClass.f90 \
FatalErrorException.o \
FTExceptionClass.o \
SMConstants.o \
SMTopographyClass.o \
EquationEvaluatorClass.o \
ProgramGlobals.o \
BiCubicClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Surfaces/DataFileTopographyClass.f90

ElementOperations.o :$(HOHQMESHPATH)/Source/MeshObjects/ElementOperations.f90 \
SMMeshClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/MeshObjects/ElementOperations.f90

Encoder.o :$(HOHQMESHPATH)/Source/Foundation/Encoder.f90
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Foundation/Encoder.f90

EquationEvaluatorClass.o :$(HOHQMESHPATH)/Source/Curves/ContinuousCurves/EquationEvaluatorClass.f90
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Curves/ContinuousCurves/EquationEvaluatorClass.f90

FatalErrorException.o :$(HOHQMESHPATH)/Source/Foundation/FatalErrorException.f90 \
FTExceptionClass.o \
FTValueClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Foundation/FatalErrorException.f90

FileAndStringProcessing.o :$(HOHQMESHPATH)/Source/IO/FileAndStringProcessing.f90 \
SMConstants.o \
ProgramGlobals.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/IO/FileAndStringProcessing.f90

fmin.o :$(HOHQMESHPATH)/Source/Curves/fmin.f90 \
SMCurveClass.o \
ProgramGlobals.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Curves/fmin.f90

Frenet.o :$(HOHQMESHPATH)/Source/3DSource/Geometry/Frenet.f90 \
SMCurveClass.o \
Geometry3D.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/3DSource/Geometry/Frenet.f90

FRSegmentedCurveClass.o :$(HOHQMESHPATH)/Source/Curves/DiscreteCurves/FRSegmentedCurveClass.f90 \
SMConstants.o \
SMCurveClass.o \
FTExceptionClass.o \
FatalErrorException.o \
FTObjectArrayClass.o \
FTLinkedListClass.o \
SizerControls.o \
ProgramGlobals.o \
Geometry.o \
ObjectArrayAdditions.o \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Curves/DiscreteCurves/FRSegmentedCurveClass.f90

FTDataClass.o : $(FTOLPATH)/Source/FTObjects/FTDataClass.f90 \
FTObjectClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTObjects/FTDataClass.f90

FTDictionaryClass.o : $(FTOLPATH)/Source/FTObjects/FTDictionaryClass.f90 \
FTObjectArrayClass.o \
FTLinkedListClass.o \
FTObjectClass.o \
FTLinkedListClass.o \
Hash.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTObjects/FTDictionaryClass.f90

FTExceptionClass.o : $(FTOLPATH)/Source/FTObjects/FTExceptionClass.f90 \
FTDictionaryClass.o \
FTValueDictionaryClass.o \
FTStackClass.o \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTObjects/FTExceptionClass.f90

FTLinkedListClass.o : $(FTOLPATH)/Source/FTObjects/FTLinkedListClass.f90 \
FTObjectArrayClass.o \
FTObjectClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTObjects/FTLinkedListClass.f90

FTMultiIndexTable.o : $(FTOLPATH)/Source/FTObjects/FTMultiIndexTable.f90 \
FTObjectClass.o \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTObjects/FTMultiIndexTable.f90

FTObjectArrayClass.o : $(FTOLPATH)/Source/FTObjects/FTObjectArrayClass.f90 \
FTObjectClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTObjects/FTObjectArrayClass.f90

FTObjectClass.o : $(FTOLPATH)/Source/FTObjects/FTObjectClass.f90
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTObjects/FTObjectClass.f90

FTOLConstants.o : $(FTOLPATH)/Source/Foundation/FTOLConstants.f90
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/Foundation/FTOLConstants.f90

FTSparseMatrixClass.o : $(FTOLPATH)/Source/FTObjects/FTSparseMatrixClass.f90 \
FTLinkedListClass.o \
FTObjectClass.o \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTObjects/FTSparseMatrixClass.f90

FTStackClass.o : $(FTOLPATH)/Source/FTObjects/FTStackClass.f90 \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTObjects/FTStackClass.f90

FTStringSetClass.o : $(FTOLPATH)/Source/FTObjects/FTStringSetClass.f90 \
FTObjectClass.o \
FTDictionaryClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTObjects/FTStringSetClass.f90

FTValueClass.o : $(FTOLPATH)/Source/FTObjects/FTValueClass.f90 \
FTOLConstants.o \
FTObjectClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTObjects/FTValueClass.f90

FTValueDictionaryClass.o : $(FTOLPATH)/Source/FTObjects/FTValueDictionaryClass.f90 \
FTValueClass.o \
FTDictionaryClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTObjects/FTValueDictionaryClass.f90

GaussianCurvature.o :$(HOHQMESHPATH)/Source/Surfaces/GaussianCurvature.f90 \
SMConstants.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Surfaces/GaussianCurvature.f90

Geometry.o :$(HOHQMESHPATH)/Source/Foundation/Geometry.f90 \
SMConstants.o \
ProgramGlobals.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Foundation/Geometry.f90

Geometry3D.o :$(HOHQMESHPATH)/Source/3DSource/Geometry/Geometry3D.f90 \
SMConstants.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/3DSource/Geometry/Geometry3D.f90

Hash.o : $(FTOLPATH)/Source/FTObjects/Hash.f90
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTObjects/Hash.f90

HexMeshObjects.o :$(HOHQMESHPATH)/Source/3DSource/HexMeshObjects.f90 \
SMConstants.o \
SMMeshObjects.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/3DSource/HexMeshObjects.f90

HOHQMeshMain.o :$(HOHQMESHPATH)/Source/HOHQMeshMain.f90 \
HOHQMesh.o \
MeshProject.o \
CommandLineReader.o \
ProgramGlobals.o \
MeshingTests.o \
MeshQualityAnalysis.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/HOHQMeshMain.f90

HOHQMesh.o :$(HOHQMESHPATH)/Source/HOHQMesh.f90 \
MeshQualityAnalysis.o \
MeshCleaner.o \
TestDataClass.o \
ControlFileReader.o \
Mesh3DOutputMethods.o \
FTValueDictionaryClass.o \
MeshProject.o \
TimerClass.o \
FTObjectArrayClass.o \
MeshOutputMethods.o \
3DMeshController.o \
MeshGeneratorMethods.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/HOHQMesh.f90

InterfaceElementMethods.o :$(HOHQMESHPATH)/Source/Mesh/InterfaceElementMethods.f90 \
MeshProject.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Mesh/InterfaceElementMethods.f90

InterpolationAndDerivatives.o :$(HOHQMESHPATH)/Source/3DSource/Geometry/InterpolationAndDerivatives.f90 \
SMConstants.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/3DSource/Geometry/InterpolationAndDerivatives.f90

LaplaceMeshSmoother.o :$(HOHQMESHPATH)/Source/Mesh/LaplaceMeshSmoother.f90 \
Connections.o \
MeshSmoother.o \
SMModel.o \
SMMeshClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Mesh/LaplaceMeshSmoother.f90

Mesh3DOutputMethods.o :$(HOHQMESHPATH)/Source/3DSource/Mesh3DOutputMethods.f90 \
HexMeshObjects.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/3DSource/Mesh3DOutputMethods.f90

MeshBoundaryMethods.o :$(HOHQMESHPATH)/Source/Mesh/MeshBoundaryMethods.f90 \
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
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Mesh/MeshBoundaryMethods.f90

MeshCleaner.o :$(HOHQMESHPATH)/Source/Mesh/MeshCleaner.f90 \
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
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Mesh/MeshCleaner.f90

MeshGeneratorMethods.o :$(HOHQMESHPATH)/Source/Mesh/MeshGeneratorMethods.f90 \
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
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Mesh/MeshGeneratorMethods.f90

MeshingTests.o :$(HOHQMESHPATH)/Source/Testing/MeshingTests.f90 \
HOHQMesh.o \
MeshProject.o \
TestDataClass.o \
TestSuiteManagerClass.o \
Encoder.o \
Assert.o \
MeshQualityAnalysis.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Testing/MeshingTests.f90

MeshOperationsModule.o :$(HOHQMESHPATH)/Source/Mesh/MeshOperationsModule.f90 \
FTObjectClass.o \
QuadTreeGridClass.o \
ProgramGlobals.o \
FTLinkedListClass.o \
SMMeshObjects.o \
SMMeshClass.o \
FTLinkedListClass.o \
SMConstants.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Mesh/MeshOperationsModule.f90

MeshOutputMethods.o :$(HOHQMESHPATH)/Source/IO/MeshOutputMethods.f90 \
FTObjectArrayClass.o \
MeshOperationsModule.o \
SMModel.o \
SMMeshObjects.o \
MeshQualityAnalysis.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/IO/MeshOutputMethods.f90

MeshProject.o :$(HOHQMESHPATH)/Source/Project/MeshProject.f90 \
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
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Project/MeshProject.f90

MeshQualityAnalysis.o :$(HOHQMESHPATH)/Source/Mesh/MeshQualityAnalysis.f90 \
SMMeshClass.o \
HexMeshObjects.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Mesh/MeshQualityAnalysis.f90

MeshSmoother.o :$(HOHQMESHPATH)/Source/Mesh/MeshSmoother.f90 \
MeshBoundaryMethods.o \
SMModel.o \
SMMeshClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Mesh/MeshSmoother.f90

Misc.o :$(HOHQMESHPATH)/Source/Foundation/Misc.f90
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Foundation/Misc.f90

NodesTemplates.o :$(HOHQMESHPATH)/Source/QuadTreeGrid/NodesTemplates.f90 \
ProgramGlobals.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/QuadTreeGrid/NodesTemplates.f90

ObjectArrayAdditions.o :$(HOHQMESHPATH)/Source/Categories/ObjectArrayAdditions.f90 \
FTObjectArrayClass.o \
FTLinkedListClass.o \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Categories/ObjectArrayAdditions.f90

ParametricEquationCurveClass.o :$(HOHQMESHPATH)/Source/Curves/ContinuousCurves/ParametricEquationCurveClass.f90 \
FTExceptionClass.o \
FTValueClass.o \
SMConstants.o \
EquationEvaluatorClass.o \
SMCurveClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Curves/ContinuousCurves/ParametricEquationCurveClass.f90

ParametricEquationTopographyClass.o :$(HOHQMESHPATH)/Source/Surfaces/ParametricEquationTopographyClass.f90 \
FTExceptionClass.o \
FTValueClass.o \
ProgramGlobals.o \
SMConstants.o \
EquationEvaluatorClass.o \
SMTopographyClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Surfaces/ParametricEquationTopographyClass.f90

ProgramGlobals.o :$(HOHQMESHPATH)/Source/Foundation/ProgramGlobals.f90 \
SMConstants.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Foundation/ProgramGlobals.f90

QuadTreeGridClass.o :$(HOHQMESHPATH)/Source/QuadTreeGrid/QuadTreeGridClass.f90 \
Sizer.o \
SMConstants.o \
SMMeshObjects.o \
FTObjectClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/QuadTreeGrid/QuadTreeGridClass.f90

QuadTreeGridGenerator.o :$(HOHQMESHPATH)/Source/QuadTreeGrid/QuadTreeGridGenerator.f90 \
Sizer.o \
QuadTreeGridClass.o \
QuadTreeTemplateOperations.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/QuadTreeGrid/QuadTreeGridGenerator.f90

QuadTreeTemplateOperations.o :$(HOHQMESHPATH)/Source/QuadTreeGrid/QuadTreeTemplateOperations.f90 \
QuadTreeGridClass.o \
SMConstants.o \
SMMeshObjects.o \
Templates.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/QuadTreeGrid/QuadTreeTemplateOperations.f90

ReaderExceptions.o :$(HOHQMESHPATH)/Source/3DSource/ReaderExceptions.f90 \
FTExceptionClass.o \
FTValueDictionaryClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/3DSource/ReaderExceptions.f90

SegmentedCurveArrayClass.o :$(HOHQMESHPATH)/Source/Curves/DiscreteCurves/SegmentedCurveArrayClass.f90 \
Geometry.o \
ProgramGlobals.o \
SMConstants.o \
FTObjectClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Curves/DiscreteCurves/SegmentedCurveArrayClass.f90

Shortcuts.o :$(HOHQMESHPATH)/Source/Foundation/Shortcuts.f90 \
FTExceptionClass.o \
FTValueDictionaryClass.o \
FatalErrorException.o \
SMConstants.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Foundation/Shortcuts.f90

SimpleSweep.o :$(HOHQMESHPATH)/Source/3DSource/SimpleSweep.f90 \
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
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/3DSource/SimpleSweep.f90

Sizer.o :$(HOHQMESHPATH)/Source/Project/Sizer/Sizer.f90 \
SizerControls.o \
ChainedSegmentedCurveClass.o \
ProgramGlobals.o \
SMConstants.o \
Geometry.o \
FTLinkedListClass.o \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Project/Sizer/Sizer.f90

SizerControls.o :$(HOHQMESHPATH)/Source/Project/Sizer/SizerControls.f90 \
FTLinkedListClass.o \
SMConstants.o \
FTObjectClass.o \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Project/Sizer/SizerControls.f90

SMChainedCurveClass.o :$(HOHQMESHPATH)/Source/Curves/ContinuousCurves/SMChainedCurveClass.f90 \
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
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Curves/ContinuousCurves/SMChainedCurveClass.f90

SMCircularArc.o :$(HOHQMESHPATH)/Source/Curves/ContinuousCurves/SMCircularArc.f90 \
SMCurveClass.o \
SMConstants.o \
ProgramGlobals.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Curves/ContinuousCurves/SMCircularArc.f90

SMConstants.o :$(HOHQMESHPATH)/Source/Foundation/SMConstants.f90
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Foundation/SMConstants.f90

SMCurveClass.o :$(HOHQMESHPATH)/Source/Curves/ContinuousCurves/SMCurveClass.f90 \
ProgramGlobals.o \
SMConstants.o \
FTObjectClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Curves/ContinuousCurves/SMCurveClass.f90

SMLine.o :$(HOHQMESHPATH)/Source/Curves/ContinuousCurves/SMLine.f90 \
SMCurveClass.o \
SMConstants.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Curves/ContinuousCurves/SMLine.f90

SMMeshClass.o :$(HOHQMESHPATH)/Source/Project/Mesh/SMMeshClass.f90 \
SegmentedCurveArrayClass.o \
FTObjectArrayClass.o \
FTLinkedListClass.o \
FTSparseMatrixClass.o \
SMMeshObjects.o \
FTObjectClass.o \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Project/Mesh/SMMeshClass.f90

SMMeshObjects.o :$(HOHQMESHPATH)/Source/MeshObjects/SMMeshObjects.f90 \
FTObjectArrayClass.o \
ProgramGlobals.o \
SMConstants.o \
FTLinkedListClass.o \
FTObjectClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/MeshObjects/SMMeshObjects.f90

SMModel.o :$(HOHQMESHPATH)/Source/Project/Model/SMModel.f90 \
SMLine.o \
FTValueDictionaryClass.o \
FatalErrorException.o \
ParametricEquationTopographyClass.o \
ProgramGlobals.o \
FTExceptionClass.o \
SweeperClass.o \
Shortcuts.o \
FTValueClass.o \
SMTopographyClass.o \
SMSplineCurveClass.o \
FTLinkedListClass.o \
Encoder.o \
SMCircularArc.o \
FTExceptionClass.o \
FTDataClass.o \
SMChainedCurveClass.o \
ParametricEquationCurveClass.o \
DataFileTopographyClass.o \
SMConstants.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Project/Model/SMModel.f90

SMSplineCurveClass.o :$(HOHQMESHPATH)/Source/Curves/ContinuousCurves/SMSplineCurveClass.f90 \
SMCurveClass.o \
SMConstants.o \
Geometry.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Curves/ContinuousCurves/SMSplineCurveClass.f90

SMTopographyClass.o :$(HOHQMESHPATH)/Source/Surfaces/SMTopographyClass.f90 \
GaussianCurvature.o \
SMConstants.o \
FTObjectClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Surfaces/SMTopographyClass.f90

SpringMeshSmoother.o :$(HOHQMESHPATH)/Source/Mesh/SpringMeshSmoother.f90 \
FatalErrorException.o \
MeshSmoother.o \
MeshBoundaryMethods.o \
SMModel.o \
FTValueDictionaryClass.o \
SMMeshClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Mesh/SpringMeshSmoother.f90

SweeperClass.o :$(HOHQMESHPATH)/Source/3DSource/SweeperClass.f90 \
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
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/3DSource/SweeperClass.f90

Templates.o :$(HOHQMESHPATH)/Source/QuadTreeGrid/Templates.f90 \
QuadTreeGridClass.o \
SMConstants.o \
SMMeshObjects.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/QuadTreeGrid/Templates.f90

TestDataClass.o :$(HOHQMESHPATH)/Source/Testing/TestDataClass.f90 \
SMConstants.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Testing/TestDataClass.f90

TestSuiteManagerClass.o : $(FTOLPATH)/Source/FTTesting/TestSuiteManagerClass.f90 \
Assert.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLPATH)/Source/FTTesting/TestSuiteManagerClass.f90

TimerClass.o :$(HOHQMESHPATH)/Source/Foundation/TimerClass.f90
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/Foundation/TimerClass.f90

TransfiniteMapClass.o :$(HOHQMESHPATH)/Source/3DSource/Geometry/TransfiniteMapClass.f90 \
CurveInterpolantClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/3DSource/Geometry/TransfiniteMapClass.f90

Utilities.o :$(HOHQMESHPATH)/Source/3DSource/Geometry/Utilities.f90 \
SMConstants.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ $(HOHQMESHPATH)/Source/3DSource/Geometry/Utilities.f90


###########
# cleanup #
###########
clean:
	rm -f *.mod
	rm -f *.o
	rm -f HOHQMesh
	rm -f HOHQMesh.exe
	rm -f Benchmarks/MeshFiles/Tests/*
	rm -f Benchmarks/PlotFiles/Tests/*
	rm -f Benchmarks/StatsFiles/Tests/*
	rm -f *.gcno
	rm -f *.gcda
	rm -f lcov.info

.PHONY: clean
