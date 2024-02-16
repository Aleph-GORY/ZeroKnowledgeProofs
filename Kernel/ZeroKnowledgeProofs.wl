(* ::Package:: *)

BeginPackage["ArmandoCruz`ZeroKnowledgeProofs`"]

(* Declare your package's public symbols here. *)

ZeroKnowledgePrivateSolution
ZeroKnowledgePublicProblem
ZeroKnowledgeCipherSolution
ZeroKnowledgeCipherProblem
ZeroKnowledgeQuery
ZeroKnowledgeResponse

GenerateZeroKnowledgeProof
GenerateZeroKnowledgeWitness
GenerateZeroKnowledgeQuery
AnswerZeroKnowledgeQuery
VerifyZeroKnowledgeProof

CompileArithmeticCircuit
EvaluateArithmeticCircuitSolution
CompileQuadraticArithmeticProgram

Begin["`Private`"]



(* ::Section:: *)
(*Objects*)


makeKeyIcon[data_, colorFunction_] := 
  ArrayPlot[
    Partition[IntegerDigits[Hash[data, "SHA256"], 4, 64], 8], 
    ImageSize -> Dynamic[{Automatic, 3.5*CurrentValue["FontCapHeight"]}], 
    ColorFunction -> colorFunction, ColorFunctionScaling -> False, 
    Frame -> None, PlotRangePadding -> None
  ]

(* 
  ZeroKnowledgePrivateSolution
  <|
    "Protocol", 
    "PrivateSolutionShape",
    "PrivateSolutionSize",
    "PublicProblemShape",
    "PublicProblemSize",
    "PrivateSolution",
    "PublicProblem"
  |>
*)
  ZeroKnowledgePrivateSolution[data_Association]["Protocol"] := data["Protocol"]
  ZeroKnowledgePrivateSolution[data_Association]["PrivateSolutionShape"] := data["PrivateSolutionShape"]
  ZeroKnowledgePrivateSolution[data_Association]["PrivateSolutionSize"] := data["PrivateSolutionSize"]
  ZeroKnowledgePrivateSolution[data_Association]["PublicProblemShape"] := data["PublicProblemShape"]
  ZeroKnowledgePrivateSolution[data_Association]["PublicProblemSize"] := data["PublicProblemSize"]

  ZeroKnowledgePrivateSolution[data_Association]["PrivateSolution"] := data["PrivateSolution"]
  ZeroKnowledgePrivateSolution[data_Association]["PublicProblem"] := data["PublicProblem"]

  ZeroKnowledgePrivateSolution /: MakeBoxes[x : ZeroKnowledgePrivateSolution[data_Association], StandardForm] := 
    BoxForm`ArrangeSummaryBox[
      ZeroKnowledgePrivateSolution, x,
      makeKeyIcon[x, 39],
      {
        BoxForm`SummaryItem[{"Protocol: ", data["Protocol"]}],
        BoxForm`SummaryItem[{"Private solution shape: ", data["PrivateSolutionShape"]}],
        BoxForm`SummaryItem[{"Private solution size: ", data["PrivateSolutionSize"]}]
      },
      {
        BoxForm`SummaryItem[{"Public problem shape: ", data["PublicProblemShape"]}],
        BoxForm`SummaryItem[{"Public problem size: ", data["PublicProblemSize"]}]
      },
      StandardForm
    ]

(* 
  ZeroKnowledgePublicProblem
  <|
    "Protocol", 
    "PublicProblemShape,"
    "PublicProblemSize", 
    "PublicProblem"
  |>
*)
  ZeroKnowledgePublicProblem[data_Association]["Protocol"] := data["Protocol"]
  ZeroKnowledgePublicProblem[data_Association]["PublicProblemShape"] := data["PublicProblemShape"]
  ZeroKnowledgePublicProblem[data_Association]["PublicProblemSize"] := data["PublicProblemSize"]

  ZeroKnowledgePublicProblem[data_Association]["PublicProblem"] := data["PublicProblem"]

  ZeroKnowledgePublicProblem /: MakeBoxes[x : ZeroKnowledgePublicProblem[data_Association], StandardForm] := 
    BoxForm`ArrangeSummaryBox[
      ZeroKnowledgePublicProblem, x,
      makeKeyIcon[x, 78],
      {
        BoxForm`SummaryItem[{"Protocol: ", data["Protocol"]}],
        BoxForm`SummaryItem[{"Public problem shape: ", data["PublicProblemShape"]}],
        BoxForm`SummaryItem[{"Public problem size: ", data["PublicProblemSize"]}]
      },{},
      StandardForm
    ]

(*
  ZeroKnowledgeCipherSolution
  <|
    "Protocol", 
    "CipherTransformation",
    "CipherSolutionsShape",
    "CipherProblemsShape",
    "CipherProblemsSize",
    "PublicCipherProblems",
    "PrivateCipherSolutions"
  |>
*)
  ZeroKnowledgeCipherSolution[data_Association]["Protocol"] := data["Protocol"]
  ZeroKnowledgeCipherSolution[data_Association]["CipherTransformation"] := data["CipherTransformation"]
  ZeroKnowledgeCipherSolution[data_Association]["CipherSolutionsShape"] := data["CipherSolutionsShape"]
  ZeroKnowledgeCipherSolution[data_Association]["CipherProblemsShape"] := data["CipherProblemsShape"]
  ZeroKnowledgeCipherSolution[data_Association]["CipherProblemsSize"] := Length[data["PublicCipherProblems"]]

  ZeroKnowledgeCipherSolution[data_Association]["PublicCipherProblems"] := data["PublicCipherProblems"]
  ZeroKnowledgeCipherSolution[data_Association]["PrivateCipherSolutions"] := data["PrivateCipherSolutions"]

  ZeroKnowledgeCipherSolution /: MakeBoxes[x : ZeroKnowledgeCipherSolution[data_Association], StandardForm] := 
    BoxForm`ArrangeSummaryBox[
      ZeroKnowledgeCipherSolution, x,
      makeKeyIcon[x, 81],
      {
        BoxForm`SummaryItem[{"Protocol: ", data["Protocol"]}],
        BoxForm`SummaryItem[{"Cipher transformation: ", data["CipherTransformation"]}],
        BoxForm`SummaryItem[{"Number of cipher problems: ", data["CipherProblemsSize"]}],
        BoxForm`SummaryItem[{"Cipher solution shape: ", data["CipherSolutionsShape"]}]
      },{
        BoxForm`SummaryItem[{"Cipher problems shape: ", data["CipherProblemsShape"]}]
      },
      StandardForm
    ]

(*
  ZeroKnowledgeCipherProblem
  <|
    "Protocol", 
    "CipherTransformation",
    "CipherProblemsShape",
    "CipherProblemsSize",
    "PublicCipherProblems"
  |>
*)
  ZeroKnowledgeCipherProblem[data_Association]["Protocol"] := data["Protocol"]
  ZeroKnowledgeCipherProblem[data_Association]["CipherTransformation"] := data["CipherTransformation"]
  ZeroKnowledgeCipherProblem[data_Association]["CipherProblemsShape"] := data["CipherProblemsShape"]
  ZeroKnowledgeCipherProblem[data_Association]["CipherProblemsSize"] := Length[data["PublicCipherProblems"]]

  ZeroKnowledgeCipherProblem[data_Association]["PublicCipherProblems"] := data["PublicCipherProblems"]

  ZeroKnowledgeCipherProblem /: MakeBoxes[x : ZeroKnowledgeCipherProblem[data_Association], StandardForm] := 
    BoxForm`ArrangeSummaryBox[
      ZeroKnowledgeCipherProblem, x,
      makeKeyIcon[x, 68],
      {
        BoxForm`SummaryItem[{"Protocol: ", data["Protocol"]}],
        BoxForm`SummaryItem[{"Cipher transformation: ", data["CipherTransformation"]}],
        BoxForm`SummaryItem[{"Number of cipher problems: ", data["CipherProblemsSize"]}],
        BoxForm`SummaryItem[{"Cipher problems shape: ", data["CipherProblemsShape"]}]
      },{},
      StandardForm
    ]

(* 
  ZeroKnowledgeQuery
  <|
    "Protocol",
    "Query",
    "QuerySize"
  |>
*)
  ZeroKnowledgeQuery[data_Association]["Protocol"] := data["Protocol"]
  ZeroKnowledgeQuery[data_Association]["QuerySize"] := Length[data["Query"]]

  ZeroKnowledgeQuery[data_Association]["Query"] := data["Query"]

  ZeroKnowledgeQuery /: MakeBoxes[x : ZeroKnowledgeQuery[data_Association], StandardForm] :=
    BoxForm`ArrangeSummaryBox[
      ZeroKnowledgeQuery, x,
      makeKeyIcon[x, 75],
      {
        BoxForm`SummaryItem[{"Protocol: ", data["Protocol"]}],
        BoxForm`SummaryItem[{"Query size: ", data["QuerySize"]}]
      },{},
      StandardForm
    ]

(*
  ZeroKnowledgeResponse
  <|"Protocol", "QuerySize", "Query", "Response"|>
*)
  ZeroKnowledgeResponse[data_Association]["Protocol"] := data["Protocol"]
  ZeroKnowledgeResponse[data_Association]["ResponseSize"] := Length[data["Response"]]
  
  ZeroKnowledgeResponse[data_Association]["Query"] := data["Query"]
  ZeroKnowledgeResponse[data_Association]["Response"] := data["Response"]

  ZeroKnowledgeResponse /: MakeBoxes[x : ZeroKnowledgeResponse[data_Association], StandardForm] := 
    BoxForm`ArrangeSummaryBox[
      ZeroKnowledgeResponse, x,
      makeKeyIcon[x, 50],
      {
        BoxForm`SummaryItem[{"Protocol: ", data["Protocol"]}],
        BoxForm`SummaryItem[{"Response size: ", data["ResponseSize"]}]
      },{},
      StandardForm
    ]



(* ::Section:: *)
(*Functions*)


(* -- Interactive zk-Proofs -- *)

(* Helper functions *)
generateGraphIsomorphism[size_] := 
  Thread[Range[size] -> RandomSample[Range[size]]]
generateSATIsomorphism[size_] := 
  Thread[Range[size] -> RandomSample[Range[size]]]

IsomorphicGraph[graph_, isomorphism_] := 
  Graph[
    EdgeList[VertexReplace[graph, isomorphism]], 
    VertexLabels -> Automatic
  ]

IsomorphicSortedGraph[graph_, isomorphism_] := 
  Graph[
    Sort /@ EdgeList[VertexReplace[graph, isomorphism]], 
    VertexLabels -> Automatic
  ]
IsomorphicHamiltonianCycle[cycle_, isomorphism_] := 
  Sort /@ Thread[
    UndirectedEdge[Association[isomorphism] /@ First /@ cycle, 
    Association[isomorphism] /@ Last /@ cycle]
  ]

(* IsomorphicBooleanFunction[boolf_, isomorphism_] := 
  ReplaceAll[boolf, i_Integer :> isomorphism[i]] *)
IsomorphicBooleanFunction[boolf_, isomorphism_] := 
  With[{permutation = Lookup[isomorphism, Range[Length[isomorphism]]]}, boolf @@ #[[permutation]] &]
IsomorphicBooleanVector[vector_, isomorphism_] := 
  Array[vector[[isomorphism[#]]] &, Length[vector]]

(* GenerateZeroKnowledgePrivateSolution  *)
GenerateZeroKnowledgePrivateSolution["Isomorphism", keySize_ : 64] := 
  Module[
    { graph = IsomorphicGraph[RandomGraph[{keySize, 4*keySize}], Thread[Range[keySize] -> Range[keySize]]], 
    isomorphism = generateGraphIsomorphism[keySize]},
    ZeroKnowledgePrivateSolution[<|
      "Protocol" -> "Isomorphism",
      "PrivateSolutionShape" -> "Isomorphism betweeen two graphs.",
      "PrivateSolutionSize" -> keySize,
      "PublicProblemShape" -> "A pair of isomorphic graphs.",
      "PublicProblemSize" -> {keySize, 4*keySize},
      "PrivateSolution" -> isomorphism,
      "PublicProblem" -> {graph, IsomorphicGraph[graph, isomorphism]}
    |>]
  ]
  
GenerateZeroKnowledgePrivateSolution["HamiltonianCycle", keySize_ : 30] :=
  Module[
    { cycle = Sort /@ EdgeList[IsomorphicGraph[CycleGraph[keySize], generateGraphIsomorphism[keySize]]], 
    graph = IsomorphicSortedGraph[RandomGraph[{keySize, 5*keySize}], Thread[Range[keySize] -> Range[keySize]]]},
    ZeroKnowledgePrivateSolution[<|
      "Protocol" -> "HamiltonianCycle",
      "PrivateSolutionSize" -> keySize,
      "PrivateSolution" -> cycle,
      "PublicProblemSize" -> {keySize, 5*keySize},
      "PublicProblem" -> 
      Graph[Union[EdgeList[graph], cycle], VertexLabels -> Automatic]
    |>]
  ]
GenerateZeroKnowledgePrivateSolution["SAT", solution_, booleanFunction_] :=
  ZeroKnowledgePrivateSolution[<|
    "Protocol" -> "SAT",
    "PrivateSolutionSize" -> Length[solution],
    "PrivateSolution" -> solution,
    "PublicProblemSize" -> Length[booleanFunction],
    "PublicProblem" -> booleanFunction
  |>]

(* GenerateZeroKnowledgeProof *)
GenerateZeroKnowledgeProof[type_, size_:Null] :=
  Module[
    {privateSolution = If[size==Null,
      GenerateZeroKnowledgePrivateSolution[type],
      GenerateZeroKnowledgePrivateSolution[type, size]
    ]},
    <|
      "ZeroKnowledgePublicProblem" -> ZeroKnowledgePublicProblem[<|
        "Protocol" -> privateSolution["Protocol"],
        "PublicProblemShape" -> privateSolution["PublicProblemShape"],
        "PublicProblemSize" -> privateSolution["PublicProblemSize"],
        "PublicProblem" -> privateSolution["PublicProblem"]
      |>],
      "ZeroKnowledgePrivateSolution" -> privateSolution
    |>
  ]
GenerateZeroKnowledgeProof[type_, solution_, problem_] :=
  Module[
    {privateSolution = GenerateZeroKnowledgePrivateSolution[type, solution, problem]},
    <|
      "ZeroKnowledgePublicProblem" -> ZeroKnowledgePublicProblem[<|
        "Protocol" -> privateSolution["Protocol"],
        "PublicProblemShape" -> privateSolution["PublicProblemShape"],
        "PublicProblemSize" -> privateSolution["PublicProblemSize"],
        "PublicProblem" -> privateSolution["PublicProblem"]
      |>],
      "ZeroKnowledgePrivateSolution" -> privateSolution
    |>
  ]

(* CipherZeroKnowledgeProof *)
CipherZeroKnowledgeProof["Isomorphism", privateSolution_] := 
  Module[
    {cipher = generateGraphIsomorphism[privateSolution["PrivateSolutionSize"]]},
    <|
      "PrivateCipherSolution" -> <|
        "Cipher" -> cipher, 
        "Solution" -> Association[privateSolution["PrivateSolution"]] /@ 
          Association[cipher] // Normal
      |>,
      "PublicCipherProblem" -> IsomorphicGraph[
        privateSolution["PublicProblem"] // First, Reverse[cipher, {2}]
      ]
    |>
  ]
CipherZeroKnowledgeProof["HamiltonianCycle", privateSolution_] := 
  Module[
    {cipher = generateGraphIsomorphism[privateSolution["PrivateSolutionSize"]]},
    <|
      "PrivateCipherSolution" -> <|
        "Cipher" -> Reverse[cipher, {2}], 
        "Solution" -> IsomorphicHamiltonianCycle[
          privateSolution["PrivateSolution"], 
          cipher
        ]
      |>,
      "PublicCipherProblem" -> IsomorphicSortedGraph[
        privateSolution["PublicProblem"], cipher
      ]
    |>
  ]
CipherZeroKnowledgeProof["SAT", privateSolution_] := 
  Module[
    {cipher = Association @ generateGraphIsomorphism[privateSolution["PrivateSolutionSize"]]},
    <|
      "PrivateCipherSolution" -> <|
        "Cipher" -> cipher,
        "Solution" -> IsomorphicBooleanVector[
          privateSolution["PrivateSolution"], Association@Reverse[Normal[cipher],{2}]
        ]
      |>,
      "PublicCipherProblem" -> IsomorphicBooleanFunction[
        privateSolution["PublicProblem"], Association@Reverse[Normal[cipher],{2}]
      ]
    |>
  ]

(* GenerateZeroKnowledgeCipherSolution *)
GenerateZeroKnowledgeCipherSolution[privateSolution_, rounds_] := 
  Module[
    {cipherProofs = Table[
      CipherZeroKnowledgeProof[privateSolution["Protocol"], privateSolution], 
      rounds
    ]},
    ZeroKnowledgeCipherSolution[<|
      "Protocol" -> privateSolution["Protocol"],
      "PrivateCipherSolutions" -> cipherProofs[[All, "PrivateCipherSolution"]],
      "PublicCipherProblems" -> cipherProofs[[All, "PublicCipherProblem"]]
    |>]
  ]

(* GenerateZeroKnowledgeWitness *)
GenerateZeroKnowledgeWitness[privateSolution_, rounds_ : 4] := 
  Module[
    {privateCipher = GenerateZeroKnowledgeCipherSolution[privateSolution, rounds]},
    <|
      "ZeroKnowledgeCipherSolution" -> privateCipher,
      "ZeroKnowledgeCipherProblem" -> ZeroKnowledgeCipherProblem[<|
        "Protocol" -> privateCipher["Protocol"],
        "PublicCipherProblems" -> privateCipher["PublicCipherProblems"]
      |>]
    |>
  ]

(* GenerateZeroKnowledgeQuery *)
ValidQueryList["Isomorphism"] := {"Cipher", "Solution"}
ValidQueryList["HamiltonianCycle"] := {"Cipher", "Solution"}
ValidQueryList["SAT"] := {"Cipher", "Solution"}

GenerateZeroKnowledgeQuery[publicWitness_] := 
  ZeroKnowledgeQuery[<|
    "Protocol" -> publicWitness["Protocol"],
    "Query" -> Table[
      RandomChoice[ValidQueryList[publicWitness["Protocol"]]], 
      publicWitness["QuerySize"]
    ]
  |>]

(* AnswerZeroKnowledgeQuery *)
AnswerZeroKnowledgeQuery["Isomorphism", cipherSolutions_, query_] := cipherSolutions @ query
AnswerZeroKnowledgeQuery["HamiltonianCycle", cipherSolutions_, query_] := cipherSolutions @ query
AnswerZeroKnowledgeQuery["SAT", cipherSolutions_, query_] := cipherSolutions @ query

(* GenerateZeroKnowledgeResponse *)
GenerateZeroKnowledgeResponse[privateCipher_, query_] := 
  ZeroKnowledgeResponse[<|
    "Protocol" -> privateCipher["Protocol"],
    "Query" -> query["Query"],
    "Response" -> Array[
      AnswerZeroKnowledgeQuery[
        privateCipher["Protocol"],
        privateCipher["PrivateCipherSolutions"][[#]], 
        query["Query"][[#]]
      ] &, 
      query["QuerySize"]
    ]
  |>]

(* VerifyZeroKnowledgeResponse  *)
VerifyZeroKnowledgeResponse["Isomorphism", publicProblem_, cipherProblem_, "Cipher", response_] := 
  IsomorphicGraph[cipherProblem, response] == (publicProblem // First)
VerifyZeroKnowledgeResponse["Isomorphism", publicProblem_, cipherProblem_, "Solution", response_] := 
  IsomorphicGraph[cipherProblem, response] == (publicProblem // Last)
 
VerifyZeroKnowledgeResponse["HamiltonianCycle", publicProblem_, cipherProblem_, "Cipher", response_] := 
  IsomorphicSortedGraph[cipherProblem, response] == publicProblem
VerifyZeroKnowledgeResponse["HamiltonianCycle", publicProblem_, cipherProblem_, "Solution", response_] := 
  And[
    SubsetQ[EdgeList[cipherProblem], response]
    (*And[(#//First)!=(#//Last)&/@response],
    Length[DeleteDuplicates[First/@response]]==First@publicProblem[
    "PublicProblemSize"]*)
  ]

VerifyZeroKnowledgeResponse["SAT", publicProblem_, cipherProblem_, "Cipher", response_] :=(
  (* Echo[cipherProblem];
  Echo[response];
  Echo[IsomorphicBooleanFunction[cipherProblem, response]];
  Echo[publicProblem]; *)
  IsomorphicBooleanFunction[cipherProblem, response] @@ List[x[#]&, Length[response]] 
  === publicProblem @@ List[x[#]&, Length[response]]
)

VerifyZeroKnowledgeResponse["SAT", publicProblem_, cipherProblem_, "Solution", response_] := 
  cipherProblem @@ response
  (* (cipherProblem /. ((Global\.b4x[#] -> response[[#]]) & /@ Range[Length[response]])) *)
  (*True*)
  (* ToExpression[StringReplace[ToString[cipherProblem], ("x[" <> ToString[#] <> "]" -> ToString[response[[#]]]) & /@ Range[Length[response]]]] *)

(* VerifyZeroKnowledgeProof  *)
VerifyZeroKnowledgeProof[publicProblem_, witness_, query_, response_] := 
  (* And@@ *)
  Array[
    Quiet@Check[VerifyZeroKnowledgeResponse[
      publicProblem["Protocol"], 
      publicProblem["PublicProblem"], 
      witness["PublicCipherProblems"][[#]], 
      query["Query"][[#]], 
      response["Response"][[#]]] &, False],
    query["QuerySize"]
  ]


(* -- zk-SNARKs -- *)

(* - Arithmetic circuits - *)
inputStyle={TreeElementStyle->LightBlue,TreeLayout->Left};
outputStyle={TreeElementStyle->LightRed,TreeLayout->Left};
arithGateRules={
	Plus:>ArithGatePlus,Times:>ArithGateTimes,
	Power[b_,e_]:>ArithGateTimes@@Table[b,e]
};

ArithGatePlus[x_]:=If[NumberQ[x],Tree[x,None],Tree[ToString[x],None,inputStyle]]
ArithGatePlus[x_Tree]:=x
ArithGatePlus[x__]:=Module[
	{lhs=ArithGatePlus@First@List@x,rhs=ArithGatePlus@@Rest@List@x,gateName=Plus},
	internalSymbolcounter=internalSymbolcounter+1;
	outputSymbol="sym"<>ToString@internalSymbolcounter;
	gate=<|"gate"->gateName,"lhs"->TreeData[lhs],
		"rhs"->TreeData[rhs],"out"->outputSymbol|>;
	circuitGates=Append[circuitGates,gate];
	Tree[outputSymbol,{Tree[gateName,{lhs,rhs}]},inputStyle]
]

ArithGateTimes[x_]:=If[NumberQ[x],Tree[x,None],Tree[ToString[x],None,inputStyle]]
ArithGateTimes[x_Tree]:=x
ArithGateTimes[x__]:=Module[
	{lhs=ArithGateTimes@First@List@x,rhs=ArithGateTimes@@Rest@List@x,gateName=Times},
	internalSymbolcounter=internalSymbolcounter+1;
	outputSymbol="sym"<>ToString@internalSymbolcounter;
	gate=<|"gate"->gateName,"lhs"->TreeData[lhs],
		"rhs"->TreeData[rhs],"out"->outputSymbol|>;
	circuitGates=Append[circuitGates,gate];
	Tree[outputSymbol,{Tree[gateName,{lhs,rhs}]},inputStyle]
]

(* CompileArithmeticCircuit *)
CompileArithmeticSubCircuit[program_,solution_Association,sufix_String]:=Module[{},
	circuitGates={};
	circuit=program/.arithGateRules;
	circuitGates=circuitGates/.TreeData[circuit]->"out"<>sufix;
	circuit=Tree["out"<>sufix,TreeChildren[circuit],outputStyle];
	circuitSolution=solution;
	Do[c=circuitGates[[i]];
	lhs=If[KeyExistsQ[circuitSolution,c["lhs"]],circuitSolution[c["lhs"]],c["lhs"]] ;rhs=If[KeyExistsQ[circuitSolution,c["rhs"]],circuitSolution[c["rhs"]],c["rhs"]] ;circuitSolution=Append[circuitSolution,c["out"]->c["gate"][lhs,rhs]];
	,{i,Length[circuitGates]}];
	internalSymbolcounter=internalSymbolcounter-1;
	<|
		"circuit"->circuit,
		"gates"->circuitGates,
		"solution"->Append[circuitSolution,"one"->1]
	|>
]

CompileArithmeticCircuit[program_,solution_Association]:=Module[{},
	internalSymbolcounter=0;
	circuit=CompileArithmeticSubCircuit[program,solution,""];
	circuit
]
CompileArithmeticCircuit[programs_List,solution_Association]:=Module[{},
	internalSymbolcounter=0;
	circuits=Table[CompileArithmeticSubCircuit[programs[[i]],solution, ToString[i]],{i,Length[programs]}];
	circuitGates=Join@@(#["gates"]&/@circuits);
	circuitSolutions=Join@@(#["solution"]&/@circuits);
	circuits=Join@@#["circuit"]&/@circuits;
	<|
		"circuit"->circuits,
		"gates"->circuitGates,
		"solution"->circuitSolutions
	|>
]

(* EvaluateArithmeticCircuitSolution *)
EvaluateArithmeticCircuitSolution[circuit_]:=Table[
	evaluation=TreeMap[If[KeyExistsQ[circuit["solution"],#],circuit["solution"][#],#]&,circuit["circuit"][[i]]];
	Tree[TreeData[evaluation],TreeChildren[evaluation],outputStyle]
	,{i,Length[circuit["circuit"]]}
]

(* - Quadratic Arithmetic Programs - *)
CalculateGateConstraint[gate_]:=If[gate["gate"]===Plus,
	<|"v"->{gate["lhs"],gate["rhs"]},"w"->{"one"},"k"->{gate["out"]}|>,
	<|"v"->{gate["lhs"]},"w"->{gate["rhs"]},"k"->{gate["out"]}|>]
CalculateConstraintSystem[circuit_]:=CalculateGateConstraint/@circuit["gates"]

ConstraintPolynomial[key_,constraints_,symbol_]:=symbol->Expand@InterpolatingPolynomial[
	Boole[MemberQ[#[key],ToString[symbol]]]&/@constraints,Global`x]
ConstraintPolynomial[key_,constraints_,"one"]:="one"->Expand@InterpolatingPolynomial[
	FirstCase[#[key],_Integer,Boole[MemberQ[#[key],"one"]]]&/@constraints,Global`x]
CalculateConstraintPolynomials[constraints_,solution_]:=Module[{},
	polynomialsV=Association[ConstraintPolynomial["v",constraints,#]&/@Keys[solution]];
	polynomialsW=Association[ConstraintPolynomial["w",constraints,#]&/@Keys[solution]];
	polynomialsK=Association[ConstraintPolynomial["k",constraints,#]&/@Keys[solution]];
	<|"v"->polynomialsV,"w"->polynomialsW,"k"->polynomialsK|>
]

(* CompileQuadraticArithmeticProgram *)
CompileQuadraticArithmeticProgram[circuit_]:=Module[{},
	gateConstraints=CalculateConstraintSystem[circuit];
	polyConstraints=CalculateConstraintPolynomials[gateConstraints,circuit["solution"]];
	V=Plus@@Expand/@Values[polyConstraints["v"]*circuit["solution"]];
	W=Plus@@Expand/@Values[polyConstraints["w"]*circuit["solution"]];
	K=Plus@@Expand/@Values[polyConstraints["k"]*circuit["solution"]];
	T=Expand[Times@@(Global`x-Range[Length[gateConstraints]])];
	F=Expand[V*W-K];
	H=PolynomialQuotient[F,T,Global`x];
	Append[polyConstraints,
		<|"V"->V,"W"->W,"K"->K,"F"->F,"T"->T,"H"->H|>]
]


End[] (* End `Private` *)

EndPackage[]
