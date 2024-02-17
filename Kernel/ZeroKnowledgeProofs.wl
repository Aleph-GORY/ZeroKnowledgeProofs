(* ::Package:: *)

BeginPackage["ArmandoCruz`ZeroKnowledgeProofs`"]

ZeroKnowledgePrivateSolution;
ZeroKnowledgePublicProblem;
ZeroKnowledgeCipherSolution;
ZeroKnowledgeCipherProblem;
ZeroKnowledgeQuery;
ZeroKnowledgeResponse;

GenerateZeroKnowledgeProof;
GenerateZeroKnowledgeWitness;
GenerateZeroKnowledgeQuery;
AnswerZeroKnowledgeQuery;
VerifyZeroKnowledgeProof;

CompileArithmeticCircuit;
EvaluateArithmeticCircuitSolution;
CompileQuadraticArithmeticProgram;


Begin["`Private`"]

<<"ArmandoCruz`ZeroKnowledgeProofs`InteractiveProofs`InteractiveProofs`"

(* ::Section:: *)
(*Objects*)

(*
  makeKeyIcon
*)
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
        BoxForm`SummaryItem[{"Protocol: ", x["Protocol"]}],
        BoxForm`SummaryItem[{"Private solution shape: ", x["PrivateSolutionShape"]}],
        BoxForm`SummaryItem[{"Private solution size: ", x["PrivateSolutionSize"]}]
      },
      {
        BoxForm`SummaryItem[{"Public problem shape: ", x["PublicProblemShape"]}],
        BoxForm`SummaryItem[{"Public problem size: ", x["PublicProblemSize"]}]
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
        BoxForm`SummaryItem[{"Protocol: ", x["Protocol"]}],
        BoxForm`SummaryItem[{"Public problem shape: ", x["PublicProblemShape"]}],
        BoxForm`SummaryItem[{"Public problem size: ", x["PublicProblemSize"]}]
      },{},
      StandardForm
    ]

(*
  ZeroKnowledgeCipherSolution
  <|
    "Protocol", 
    "CipherTransformation",
    "CipherSolutionShape",
    "CipherSolutionSize",
    "CipherProblemShape",
    "CipherProblemSize",
    "PublicCipherProblem",
    "PrivateCipherSolution"
  |>
*)
  ZeroKnowledgeCipherSolution[data_Association]["Protocol"] := data["Protocol"]
  ZeroKnowledgeCipherSolution[data_Association]["CipherTransformation"] := data["CipherTransformation"]
  ZeroKnowledgeCipherSolution[data_Association]["CipherSolutionShape"] := data["CipherSolutionShape"]
  ZeroKnowledgeCipherSolution[data_Association]["CipherSolutionSize"] := Length[data["PrivateCipherSolution"]]
  ZeroKnowledgeCipherSolution[data_Association]["CipherProblemShape"] := data["CipherProblemShape"]
  ZeroKnowledgeCipherSolution[data_Association]["CipherProblemSize"] := Length[data["PublicCipherProblem"]]

  ZeroKnowledgeCipherSolution[data_Association]["PublicCipherProblem"] := data["PublicCipherProblem"]
  ZeroKnowledgeCipherSolution[data_Association]["PrivateCipherSolution"] := data["PrivateCipherSolution"]

  ZeroKnowledgeCipherSolution /: MakeBoxes[x : ZeroKnowledgeCipherSolution[data_Association], StandardForm] := 
    BoxForm`ArrangeSummaryBox[
      ZeroKnowledgeCipherSolution, x,
      makeKeyIcon[x, 81],
      {
        BoxForm`SummaryItem[{"Protocol: ", x["Protocol"]}],
        BoxForm`SummaryItem[{"Cipher transformation: ", x["CipherTransformation"]}],
        BoxForm`SummaryItem[{"Number of cipher problems: ", x["CipherProblemSize"]}],
        BoxForm`SummaryItem[{"Cipher solution shape: ", x["CipherSolutionShape"]}]
      },{
        BoxForm`SummaryItem[{"Cipher problems shape: ", x["CipherProblemShape"]}]
      },
      StandardForm
    ]

(*
  ZeroKnowledgeCipherProblem
  <|
    "Protocol", 
    "CipherTransformation",
    "CipherProblemShape",
    "CipherProblemSize",
    "PublicCipherProblem"
  |>
*)
  ZeroKnowledgeCipherProblem[data_Association]["Protocol"] := data["Protocol"]
  ZeroKnowledgeCipherProblem[data_Association]["CipherTransformation"] := data["CipherTransformation"]
  ZeroKnowledgeCipherProblem[data_Association]["CipherProblemShape"] := data["CipherProblemShape"]
  ZeroKnowledgeCipherProblem[data_Association]["CipherProblemSize"] := Length[data["PublicCipherProblem"]]

  ZeroKnowledgeCipherProblem[data_Association]["PublicCipherProblem"] := data["PublicCipherProblem"]

  ZeroKnowledgeCipherProblem /: MakeBoxes[x : ZeroKnowledgeCipherProblem[data_Association], StandardForm] := 
    BoxForm`ArrangeSummaryBox[
      ZeroKnowledgeCipherProblem, x,
      makeKeyIcon[x, 68],
      {
        BoxForm`SummaryItem[{"Protocol: ", x["Protocol"]}],
        BoxForm`SummaryItem[{"Cipher transformation: ", x["CipherTransformation"]}],
        BoxForm`SummaryItem[{"Number of cipher problems: ", x["CipherProblemSize"]}],
        BoxForm`SummaryItem[{"Cipher problems shape: ", x["CipherProblemShape"]}]
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
        BoxForm`SummaryItem[{"Protocol: ", x["Protocol"]}],
        BoxForm`SummaryItem[{"Query size: ", x["QuerySize"]}]
      },{},
      StandardForm
    ]

(*
  ZeroKnowledgeResponse
  <|
    "Protocol",
    "ResponseSize",
    "Query",
    "Response"
  |>
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
        BoxForm`SummaryItem[{"Protocol: ", x["Protocol"]}],
        BoxForm`SummaryItem[{"Response size: ", x["ResponseSize"]}]
      },{},
      StandardForm
    ]



(* ::Section:: *)
(*Functions*)


(* 
  GenerateZeroKnowledgeProof
*)
  GenerateZeroKnowledgeProof[protocol_, shape_:Null] :=
    Module[
      {privateSolution = GenerateZeroKnowledgePrivateSolution[protocol, shape]},
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
  GenerateZeroKnowledgeProof[protocol_, solution_] :=
    Module[
      {privateSolution = GenerateZeroKnowledgePrivateSolution[protocol, solution]},
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

(*
  GenerateCipherSolution
*)
  GenerateCipherSolution[privateSolution_, opts: OptionsPattern[]] := 
    Module[
      {
        cipherSolution = CipherPrivateSolution[
          privateSolution["Protocol"], 
          privateSolution, 
          opts
        ]
      },
      ZeroKnowledgeCipherSolution[cipherSolution]
    ]

(* 
  GenerateZeroKnowledgeWitness
*)
  GenerateZeroKnowledgeWitness[privateSolution_, opts: OptionsPattern[]] := 
    Module[
      {cipherSolution = GenerateCipherSolution[privateSolution, opts]},
      <|
        "ZeroKnowledgeCipherProblem" -> ZeroKnowledgeCipherProblem[<|
          "Protocol" -> cipherSolution["Protocol"],
          "CipherTransformation" -> cipherSolution["CipherTransformation"],
          "CipherProblemShape" -> cipherSolution["CipherProblemShape"],
          "PublicCipherProblem" -> cipherSolution["PublicCipherProblem"]
        |>],
        "ZeroKnowledgeCipherSolution" -> cipherSolution
      |>
    ]

(* 
  VerifyZeroKnowledgeProof
*)
  VerifyZeroKnowledgeProof[publicProblem_, witness_, opts: OptionsPattern[]] := 
    VerifyZeroKnowledgeProof[publicProblem["Protocol"], publicProblem, witness, opts]

End[]

EndPackage[]
