//
//  EquationalReasoning.swift
//  SwiftCheck
//
//  Created by Robert Widmann on 9/22/16.
//  Copyright © 2016 Typelift. All rights reserved.
//

struct Equation {
	let lhs : Term
	let rhs : Term
}

struct TypedEquation<A> {
	let lhs : Expr<A>
	let rhs : Expr<A>
}

struct Context {
	let rel : CCS
	let maxDepth : Int
	let universe : Dictionary<Int, Universe>

	init(rel : CCS, maxDepth : Int, universe : Dictionary<Int, Universe>) {
		self.rel = rel
		self.maxDepth = maxDepth
		self.universe = universe
	}

	init(_ d : Int, _ syms : [Symbol], _ ts : [Tagged<Term>]) {
		/*

		initial d syms ts =
		let n = 1+maximum (0:map index syms)
		(universe, rel) =
		CC.runCC (CC.initial n) $
		forM (partitionBy (witnessType . tag) ts) $ \xs@(x:_) ->
		fmap (witnessType (tag x),) (createUniverse (map erase xs))
		univMap = Map.fromList universe

		in Context rel d . IntMap.fromList $ [
		(index sym,
		Map.findWithDefault IntMap.empty (symbolType sym) univMap)
		| sym <- syms ]
		*/
		fatalError()
	}
}

typealias Universe = Dictionary<Int, [Int]>

/// The State Monad represents a computation that threads a piece of state
/// through each step.
public struct State<S, A> {
	public let runState : (S) -> (A, S)

	/// Creates a new State Monad given a function from a piece of state to a
	/// value and an updated state.
	public init(_ runState : @escaping (S) -> (A, S)) {
		self.runState = runState
	}

	/// Evaluates the computation given an initial state then returns a final
	/// value after running each step.
	public func eval(s : S) -> A {
		return self.runState(s).0
	}

	/// Evaluates the computation given an initial state then returns the final
	/// state after running each step.
	public func exec(s : S) -> S {
		return self.runState(s).1
	}

	/// Executes an action that can modify the inner state.
	public func withState(_ f : @escaping (S) -> S) -> State<S, A> {
		return State({ x in self.runState(f(x)) })
	}

	public func fmap<B>(_ f : @escaping (A) -> B) -> State<S, B> {
		return State<S, B> { s in
			let (val, st2) = self.runState(s)
			return (f(val), st2)
		}
	}
	
	public static func pure(_ x : A) -> State<S, A> {
		return State { s in (x, s) }
	}

	public func bind<B>(_ f : @escaping (A) -> State<S, B>) -> State<S, B> {
		return State<S, B> { s in
			let (a, s2) = self.runState(s)
			return f(a).runState(s2)
		}
	}
}

/// Fetches the current value of the state.
public func get<S>() -> State<S, S> {
	return State<S, S> { ($0, $0) }
}

/// Sets the state.
public func put<S>(s : S) -> State<S, ()> {
	return State<S, ()> { _ in ((), s) }
}

/// Gets a specific component of the state using a projection function.
public func gets<S, A>(_ f : @escaping (S) -> A) -> State<S, A> {
	return State { s in (f(s), s) }
}

/// Updates the state with the result of executing the given function.
public func modify<S>(_ f : @escaping (S) -> S) -> State<S, ()> {
	return State<S, ()> { s in ((), f(s)) }
}

typealias EQ<A> = (Int, Dictionary<Int, Universe>) -> State<CCS, A> // CC<A>

func pure<A>(_ x : A) -> EQ<A> {
	return { (_, _) in
		return State<CCS, A>.pure(x)
	}
}

func map<A, B>(_ f : @escaping (A) -> B, _ e : @escaping EQ<A>) -> EQ<B> {
	return { (bi, bu) in
		return e(bi, bu).fmap(f)
	}
}

func bind<A, B>(_ m : @escaping EQ<A>, _ f : @escaping (A) -> EQ<B>) -> EQ<B> {
	return { (bi, bu) in
		return m(bi, bu).bind { a in
			return f(a)(bi, bu)
		}
	}
}

func createUniverse(_ ts : [Term]) -> CC<Universe> {
	/*
	createUniverse ts = fmap IntMap.fromList (mapM createTerms tss)
	where tss = partitionBy depth ts
	createTerms ts@(t:_) = fmap (depth t,) (mapM flatten ts)

	*/
	fatalError()
}

func runEQ<A>(_ ctx : Context, _ x : EQ<A>) -> (A, Context) {
	let (y, rel2) = x(ctx.maxDepth, ctx.universe).runState(ctx.rel)
	let ctx2 = Context(rel: rel2, maxDepth: ctx.maxDepth, universe: ctx.universe)
	return (y, ctx2)
}

func evalEQ<A>(_ ctx : Context, _ x : EQ<A>) -> A {
	return runEQ(ctx, x).0
}

func execEQ<A>(_ ctx : Context, _ x : EQ<A>) -> Context {
	return runEQ(ctx, x).1
}

func liftCC<A>(_ x : CC<A>) -> EQ<A> {
	return { _ in x }
}

infix operator =?=
infix operator =%=

func =?=(t : Term, u : Term) -> EQ<Bool> {
	return liftCC(flatten(t).bind { x in
		return flatten(u).bind { y in
			return x =?= y
		}
	})
}

func equal(_ eq : Equation) -> EQ<Bool> {
	return eq.lhs =?= eq.rhs
}

func =%=(t : Term, u : Term) -> EQ<Bool> {
	return unify(Equation(lhs: t, rhs: u))
}

func unify(_ eq : Equation) -> EQ<Bool> {
	return { (d, ctx) in
		return (eq.lhs =?= eq.rhs)(d, ctx).bind { b in
			if !b {
				(substs(eq.lhs, ctx, d) + substs(eq.rhs, ctx, d)).forEach { s in
					_ = liftCC(subst(s, eq.lhs).bind { t2 in
						return subst(s, eq.rhs).bind { u2 in
							return t2 =%= u2
						}
					})
				}
			}
			return CC<Bool>.pure(b)
		}
	}
}
/*
      putLookup (insert2 x' f' c m)
      addFunUses [(x', c)] f'
      addArgUses [(f', c)] x'
      invariant (printf "after %s$$%s" (show f) (show x))
      return c
*/
typealias Subst = (Symbol) -> Int

func substs(_ t : Term, _ univ : Dictionary<Int, Universe>, _ d : Int) -> [Subst] {
	/*
	substs t univ d = map lookup (sequence (map choose vars))
	where vars = map (maximumBy (comparing snd)) .
	partitionBy fst .
	holes $ t
	*/
	fatalError()
}

/*
choose (x, n) =
let m = IntMap.findWithDefault (ERROR "empty universe")
(index x) univ in
[ (x, t)
| d' <- [0..d-n],
t <- IntMap.findWithDefault [] d' m ]

lookup ss =
let m = IntMap.fromList [ (index x, y) | (x, y) <- ss ]
in \x -> IntMap.findWithDefault (index x) (index x) m
*/

func subst(_ s : @escaping Subst, _ t : Term) -> CC<Int> {
	switch t {
	case let .Var(x):
		return CC.pure(s(x))
	case let .Const(x):
		return CC.pure(x.index)
	case let .App(f, x):
		return subst(s, f).bind { f2 in
			return subst(s, x).bind { x2 in
				return f2 %% x2
			}
		}
	}
}

func flatten(_ t : Term) -> CC<Int> {
	return subst({ x in x.index }, t)
}

//get :: EQ CC.S
//get = liftCC S.get
//
//put :: CC.S -> EQ ()
//put x = liftCC (S.put x)
//
//rep :: Term -> EQ Int
//rep x = liftCC (flatten x >>= CC.rep)
