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

<<"ArmandoCruz`ZeroKnowledgeProofs`Symbols`"
<<"ArmandoCruz`ZeroKnowledgeProofs`InteractiveProofs`InteractiveProofs`"
<<"ArmandoCruz`ZeroKnowledgeProofs`zkSNARK`zkSNARK`"


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
