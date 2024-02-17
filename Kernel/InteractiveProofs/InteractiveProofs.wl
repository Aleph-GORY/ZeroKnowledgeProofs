(* ::Package:: *)
(* 
  # Interactive zkProofs
  Implementation of interactive zero knowledge proofs
*)

BeginPackage["ArmandoCruz`ZeroKnowledgeProofs`InteractiveProofs`InteractiveProofs`"]


Get["ArmandoCruz`ZeroKnowledgeProofs`InteractiveProofs`Protocols`Isomorphism`"]

Begin["`Private`"]

(* ::Section:: *)
(*Definitions*)

(*
  GenerateZeroKnowledgeQuery
*)
GenerateZeroKnowledgeQuery[cipherProblem_] := 
  ZeroKnowledgeQuery[<|
    "Protocol" -> cipherProblem["Protocol"],
    "Query" -> Table[
      RandomChoice[ValidQueryList[cipherProblem["Type"]]], 
      cipherProblem["Rounds"]
    ],
    "QuerySize" -> 0
  |>]

(*
  AnswerZeroKnowledgeQuery
*)
  AnswerZeroKnowledgeQuery[cipherSolution_, query_] := 
    ZeroKnowledgeResponse[<|
      "Protocol" -> cipherSolution["Protocol"],
      "ResponseSize" -> query["QuerySize"],
      "Query" -> query["Query"],
      "Response" -> Array[
        AnswerZeroKnowledgeQuery[cipherSolution["Protocol"],
          cipherSolution["PrivateCipherSolution"][[#]], 
          query["Query"][[#]]
        ] &, 
        query["QuerySize"]
      ]
    |>]

(*
  VerifyInteractiveProof
*)
VerifyInteractiveProof[publicProblem_, witness_, query_, response_] := 
  Array[
    Quiet@Check[
      VerifyZeroKnowledgeResponse[
        publicProblem["Protocol"], 
        publicProblem["PublicProblem"], 
        witness["PublicCipherProblems"][[#]], 
        query["Queries"][[#]], 
        response["Responses"][[#]]
      ] &,
      False
    ],
    query["Rounds"]
  ]

End[]

EndPackage[]
