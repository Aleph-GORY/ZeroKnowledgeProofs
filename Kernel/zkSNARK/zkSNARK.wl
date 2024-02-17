(* ::Package:: *)

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

