(* ::Package:: *)

BeginPackage["ArmandoCruz`ZeroKnowledgeProofs`"]

(* Declare your package's public symbols here. *)

ZeroKnowledgePrivateSolution
ZeroKnowledgePublicProblem
ZeroKnowledgePrivateCipher
ZeroKnowledgePublicWitness
ZeroKnowledgeQuery
ZeroKnowledgeResponse

GenerateZeroKnowledgeProof
GenerateZeroKnowledgeProver
GenerateZeroKnowledgeQuery
GenerateZeroKnowledgeResponse
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

(* ZeroKnowledgePrivateSolution *)
(* <|"Type", "PrivateSolutionSize", "PrivateSolution", "PublicProblemSize", "PublicProblem"|> *)
ZeroKnowledgePrivateSolution[data_Association]["Type"] := data["Type"]
ZeroKnowledgePrivateSolution[data_Association]["PrivateSolutionSize"] := data["PrivateSolutionSize"]
ZeroKnowledgePrivateSolution[data_Association]["PrivateSolution"] := data["PrivateSolution"]
ZeroKnowledgePrivateSolution[data_Association]["PublicProblemSize"] := data["PublicProblemSize"]
ZeroKnowledgePrivateSolution[data_Association]["PublicProblem"] := data["PublicProblem"]

ZeroKnowledgePrivateSolution /: MakeBoxes[x : ZeroKnowledgePrivateSolution[data_Association], StandardForm] := 
  BoxForm`ArrangeSummaryBox[
    ZeroKnowledgePrivateSolution, x,
    makeKeyIcon[x, 39],
    {
      BoxForm`SummaryItem[{"Type: ", data["Type"]}],
      BoxForm`SummaryItem[{"Private solution size: ", data["PrivateSolutionSize"]}],
      BoxForm`SummaryItem[{"Public problem size: ", data["PublicProblemSize"]}]
    },
    {
      BoxForm`SummaryItem[{"Private solution: ", data["PrivateSolution"]}],
      BoxForm`SummaryItem[{"Public problem: ", data["PublicProblem"]}]
    },
    StandardForm
  ]

(* ZeroKnowledgePublicProblem *)
(* <|"Type", "PublicProblemSize", "PublicProblem"|> *)
ZeroKnowledgePublicProblem[data_Association]["Type"] := data["Type"]
ZeroKnowledgePublicProblem[data_Association]["PublicProblemSize"] := data["PublicProblemSize"]
ZeroKnowledgePublicProblem[data_Association]["PublicProblem"] := data["PublicProblem"]

ZeroKnowledgePublicProblem /: MakeBoxes[x : ZeroKnowledgePublicProblem[data_Association], StandardForm] := 
  BoxForm`ArrangeSummaryBox[
    ZeroKnowledgePublicProblem, x,
    makeKeyIcon[x, 78],
    {
      BoxForm`SummaryItem[{"Type: ", data["Type"]}],
      BoxForm`SummaryItem[{"Public problem size: ", data["PublicProblemSize"]}]
    },
    {
      BoxForm`SummaryItem[{"Public problem: ", data["PublicProblem"]}]
    },
    StandardForm
  ]

(* ZeroKnowledgePrivateCipher *)
(* <|"Type", "Rounds", "PrivateCipherSolutions", "PublicCipherProblems"|> *)
ZeroKnowledgePrivateCipher[data_Association]["Type"] := data["Type"]
ZeroKnowledgePrivateCipher[data_Association]["PrivateCipherSolutions"] := data["PrivateCipherSolutions"]
ZeroKnowledgePrivateCipher[data_Association]["PublicCipherProblems"] := data["PublicCipherProblems"]
ZeroKnowledgePrivateCipher[data_Association]["Rounds"] := Length[data["PublicCipherProblems"]]

ZeroKnowledgePrivateCipher /: MakeBoxes[x : ZeroKnowledgePrivateCipher[data_Association], StandardForm] := 
  BoxForm`ArrangeSummaryBox[
    ZeroKnowledgePrivateCipher, x,
    makeKeyIcon[x, 81],
    {
      BoxForm`SummaryItem[{"Type: ", data["Type"]}],
      BoxForm`SummaryItem[{"Number of rounds: ", x["Rounds"]}]
    },
    {
      BoxForm`SummaryItem[{"First private cipher solution: ", First @ data["PrivateCipherSolutions"]}],
      BoxForm`SummaryItem[{"First public CipherProblem: ", First @ data["PublicCipherProblems"]}]
    },
    StandardForm
  ]

(* ZeroKnowledgePublicWitness *)
(* <|"Type", "Rounds", "PublicCipherProblems"|> *)
ZeroKnowledgePublicWitness[data_Association]["Type"] := data["Type"]
ZeroKnowledgePublicWitness[data_Association]["PublicCipherProblems"] := data["PublicCipherProblems"]
ZeroKnowledgePublicWitness[data_Association]["Rounds"] := Length[data["PublicCipherProblems"]]

ZeroKnowledgePublicWitness /: MakeBoxes[x : ZeroKnowledgePublicWitness[data_Association], StandardForm] := 
  BoxForm`ArrangeSummaryBox[
    ZeroKnowledgePublicWitness, x,
    makeKeyIcon[x, 68],
    {
      BoxForm`SummaryItem[{"Type: ", data["Type"]}],
      BoxForm`SummaryItem[{"Number of rounds: ", x["Rounds"]}]
    },
    {
      BoxForm`SummaryItem[{"First public CipherProblem: ", First @ data["PublicCipherProblems"]}]
    },
    StandardForm
  ]

(* ZeroKnowledgeQuery *)
(* <|"Type", "Rounds", "Queries"|> *)
ZeroKnowledgeQuery[data_Association]["Type"] := data["Type"]
ZeroKnowledgeQuery[data_Association]["Queries"] := data["Queries"]
ZeroKnowledgeQuery[data_Association]["Rounds"] := Length[data["Queries"]]

ZeroKnowledgeQuery /: MakeBoxes[x : ZeroKnowledgeQuery[data_Association], StandardForm] :=
  BoxForm`ArrangeSummaryBox[
    ZeroKnowledgeQuery, x,
    makeKeyIcon[x, 75],
    {
      BoxForm`SummaryItem[{"Type: ", data["Type"]}],
      BoxForm`SummaryItem[{"Number of rounds: ", x["Rounds"]}]
    },
    {
      BoxForm`SummaryItem[{"First query: ", First @ data["Queries"]}]
    },
    StandardForm
  ]

(* ZeroKnowledgeResponse *)
(* <|"Type", "Rounds", "Queries", "Responses"|> *)
ZeroKnowledgeResponse[data_Association]["Type"] := data["Type"]
ZeroKnowledgeResponse[data_Association]["Queries"] := data["Queries"]
ZeroKnowledgeResponse[data_Association]["Responses"] := data["Responses"]
ZeroKnowledgeResponse[data_Association]["Rounds"] := Length[data["Responses"]]

ZeroKnowledgeResponse /: MakeBoxes[x : ZeroKnowledgeResponse[data_Association], StandardForm] := 
  BoxForm`ArrangeSummaryBox[
    ZeroKnowledgeResponse, x,
    makeKeyIcon[x, 50],
    {
      BoxForm`SummaryItem[{"Type: ", data["Type"]}],
      BoxForm`SummaryItem[{"Number of rounds: ", x["Rounds"]}]
      },
    {
      BoxForm`SummaryItem[{"First query: ", First @ data["Queries"]}],
      BoxForm`SummaryItem[{"First response: ", First @ data["Responses"]}]
      },
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
GenerateZeroKnowledgePrivateSolution["Isomorphism", keySize_ : 30] := 
  Module[
    { graph = IsomorphicGraph[RandomGraph[{keySize, 5*keySize}], Thread[Range[keySize] -> Range[keySize]]], 
    isomorphism = generateGraphIsomorphism[keySize]},
    ZeroKnowledgePrivateSolution[<|
      "Type" -> "Isomorphism",
      "PrivateSolutionSize" -> keySize,
      "PrivateSolution" -> isomorphism,
      "PublicProblemSize" -> {keySize, 5*keySize},
      "PublicProblem" -> {graph, IsomorphicGraph[graph, isomorphism]}
    |>]
  ]
GenerateZeroKnowledgePrivateSolution["HamiltonianCycle", keySize_ : 30] :=
  Module[
    { cycle = Sort /@ EdgeList[IsomorphicGraph[CycleGraph[keySize], generateGraphIsomorphism[keySize]]], 
    graph = IsomorphicSortedGraph[RandomGraph[{keySize, 5*keySize}], Thread[Range[keySize] -> Range[keySize]]]},
    ZeroKnowledgePrivateSolution[<|
      "Type" -> "HamiltonianCycle",
      "PrivateSolutionSize" -> keySize,
      "PrivateSolution" -> cycle,
      "PublicProblemSize" -> {keySize, 5*keySize},
      "PublicProblem" -> 
      Graph[Union[EdgeList[graph], cycle], VertexLabels -> Automatic]
    |>]
  ]
GenerateZeroKnowledgePrivateSolution["SAT", solution_, booleanFunction_] :=
  ZeroKnowledgePrivateSolution[<|
    "Type" -> "SAT",
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
      "ZeroKnowledgePrivateSolution" -> privateSolution,
      "ZeroKnowledgePublicProblem" -> ZeroKnowledgePublicProblem[<|
        "Type" -> privateSolution["Type"],
        "PublicProblemSize" -> privateSolution["PublicProblemSize"],
        "PublicProblem" -> privateSolution["PublicProblem"]
      |>]
    |>
  ]
GenerateZeroKnowledgeProof[type_, solution_, problem_] :=
  Module[
    {privateSolution = GenerateZeroKnowledgePrivateSolution[type, solution, problem]},
    <|
      "ZeroKnowledgePrivateSolution" -> privateSolution,
      "ZeroKnowledgePublicProblem" -> ZeroKnowledgePublicProblem[<|
        "Type" -> privateSolution["Type"],
        "PublicProblemSize" -> privateSolution["PublicProblemSize"],
        "PublicProblem" -> privateSolution["PublicProblem"]
      |>]
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

(* GenerateZeroKnowledgePrivateCipher *)
GenerateZeroKnowledgePrivateCipher[privateSolution_, rounds_] := 
  Module[
    {cipherProofs = Table[
      CipherZeroKnowledgeProof[privateSolution["Type"], privateSolution], 
      rounds
    ]},
    ZeroKnowledgePrivateCipher[<|
      "Type" -> privateSolution["Type"],
      "PrivateCipherSolutions" -> cipherProofs[[All, "PrivateCipherSolution"]],
      "PublicCipherProblems" -> cipherProofs[[All, "PublicCipherProblem"]]
    |>]
  ]

(* GenerateZeroKnowledgeProver *)
GenerateZeroKnowledgeProver[privateSolution_, rounds_ : 4] := 
  Module[
    {privateCipher = GenerateZeroKnowledgePrivateCipher[privateSolution, rounds]},
    <|
      "ZeroKnowledgePrivateCipher" -> privateCipher,
      "ZeroKnowledgePublicWitness" -> ZeroKnowledgePublicWitness[<|
        "Type" -> privateCipher["Type"],
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
    "Type" -> publicWitness["Type"],
    "Queries" -> Table[
      RandomChoice[ValidQueryList[publicWitness["Type"]]], 
      publicWitness["Rounds"]
    ]
  |>]

(* AnswerZeroKnowledgeQuery *)
AnswerZeroKnowledgeQuery["Isomorphism", cipherSolutions_, query_] := cipherSolutions @ query
AnswerZeroKnowledgeQuery["HamiltonianCycle", cipherSolutions_, query_] := cipherSolutions @ query
AnswerZeroKnowledgeQuery["SAT", cipherSolutions_, query_] := cipherSolutions @ query

(* GenerateZeroKnowledgeResponse *)
GenerateZeroKnowledgeResponse[privateCipher_, query_] := 
  ZeroKnowledgeResponse[<|
    "Type" -> privateCipher["Type"],
    "Queries" -> query["Queries"],
    "Responses" -> Array[
      AnswerZeroKnowledgeQuery[
        privateCipher["Type"],
        privateCipher["PrivateCipherSolutions"][[#]], 
        query["Queries"][[#]]
      ] &, 
      query["Rounds"]
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
      publicProblem["Type"], 
      publicProblem["PublicProblem"], 
      witness["PublicCipherProblems"][[#]], 
      query["Queries"][[#]], 
      response["Responses"][[#]]] &, False],
    query["Rounds"]
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
