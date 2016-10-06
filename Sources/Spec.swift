//
//  Spec.swift
//  SwiftCheck
//
//  Created by Robert Widmann on 9/22/16.
//  Copyright © 2016 Typelift. All rights reserved.
//

typealias Fingerprint = Int

infix operator • : NilCoalescingPrecedence

private func • <A, B, C>(f : @escaping (B) -> C, g : @escaping (A) -> B) -> (A) -> C {
	return { f(g($0)) }
}

func filterLM<T>(_ xs : [T], _ p : @escaping (T) -> EQ<Bool>) -> EQ<[T]> {
	return xs.reduce(pure([T]())) { (acc : @escaping EQ<[T]>, x : T) -> EQ<[T]> in
		return bind(p(x), { b in b ? bind(acc, { (y : [T]) in pure(y + [x]) }) : acc })
	}
}

func prune<A>(_ ctx : Context, _ reps : [Term], _ erase : @escaping (A) -> Equation, _ eqs : [A]) -> [A] {
	func provable(_ eq : Equation) -> EQ<Bool> {
		return bind((eq.lhs =?= eq.rhs), { (res : Bool) in
			if res {
				return { _ in State<CCS, Bool>.pure(true) }
			}
			/*
			state <- get
			-- Check that we won't unify two representatives---if we do
			-- the equation is false
			t =:= u
			reps' <- mapM rep reps
			if sort reps' == usort reps' then return False else do
			put state
			return True
			*/
			fatalError()
		})
	}
	let vv : EQ<[A]> = filterLM(eqs, { (x : A) -> EQ<Bool> in map({ b in !b }, provable(erase(x))) })
	return evalEQ(ctx, vv)
}

/*
defines :: Equation -> Maybe Symbol
defines (t :=: u) = do
  let isVar Var{} = True
      isVar _ = False

      acyclic t =
        all acyclic (args t) &&
        case functor t == functor u of
          True -> usort (map Var (vars t)) `isProperSubsetOf` args u
          False -> True
      xs `isProperSubsetOf` ys = xs `isSubsetOf` ys && sort xs /= sort ys
      xs `isSubsetOf` ys = sort xs `isSublistOf` sort ys
      [] `isSublistOf` _ = True
      (x:xs) `isSublistOf` [] = False
      (x:xs) `isSublistOf` (y:ys)
        | x == y = xs `isSublistOf` ys
        | otherwise = (x:xs) `isSublistOf` ys

  guard (all isVar (args u) && usort (args u) == args u &&
         acyclic t && vars t `isSubsetOf` vars u)

  return (functor u)

definitions :: [Equation] -> [Equation]
definitions es = [ e | e <- es, defines e /= Nothing ]


*/

func runTool<A : Signature>(_ tool : (Sig) -> (), _ sig : A) {
	print("== API ==")
	print(A.signature.description)
	return tool(A.signature)
}

enum Target {
	case SymbolTarget(Symbol)
	case NoTarget
}

struct TyCon {
	let name : String
}

protocol Typeable {
	static var type : TypeRep { get }
}

indirect enum TypeRep : Equatable, Comparable, Hashable {
	case Rep(ObjectIdentifier, TyCon, [TypeRep])

	var hashValue : Int {
		switch self {
		case let .Rep(i, _, _):
			return i.hashValue
		}
	}

	static func ==(l : TypeRep, r : TypeRep) -> Bool {
		switch (l, r) {
		case let (.Rep(i, _, _), .Rep(j, _, _)):
			return i == j
		}
	}

	static func <(l : TypeRep, r : TypeRep) -> Bool {
		switch (l, r) {
		case let (.Rep(i, _, _), .Rep(j, _, _)):
			return i < j
		}
	}
}

struct Symbol : Equatable, Comparable, Hashable, CustomStringConvertible {
	let index : Int
	let name : String
	let arity : Int
	let silent : Bool
	let undef : Bool
	let type : TypeRep

	init<A>(_ x : String, _ arity : Int, _ v : A) { // symbol
		self.index = 0
		self.name = x
		self.arity = arity
		self.silent = false
		self.undef = false
		self.type = .Rep(ObjectIdentifier(A.self), TyCon(name: String(describing: A.self)), [])
	}

	var description : String {
		return self.name
	}

	var hashValue : Int {
		return self.index
	}

	static func ==(l : Symbol, r : Symbol) -> Bool {
		return l.index == r.index
	}

	static func <(l : Symbol, r : Symbol) -> Bool {
		return l.index < r.index
	}
}

indirect enum Term {
	case Var(Symbol)
	case Const(Symbol)
	case App(Term, Term)

	var symbols : [Symbol] {
		switch self {
		case let .Var(x):
			return [x]
		case let .Const(x):
			return [x]
		case let .App(f, x):
			return f.symbols + x.symbols
		}
	}

	var depth : Int {
		switch self {
		case let .App(f, x):
			return max(f.depth, 1 + x.depth)
		default:
			return 1
		}
	}

	func size(_ v : Int) -> Int {
		switch self {
		case .Var(_):
			return v
		case .Const(_):
			return 1
		case let .App(f, x):
			return f.size(v) + x.size(v)
		}
	}

	var holes : [(Symbol, Int)] {
		func go(_ d : Int, _ t : Term) -> [(Symbol, Int)] {
			switch t {
			case let .Var(x):
				return [(x, d)]
			case .Const(_):
				return []
			case let .App(f, x):
				return go(d, f) + go(d + 1, x)
			}
		}
		return go(0, self)
	}

	var functor : Symbol {
		switch self {
		case let .Var(x):
			return x
		case let .Const(x):
			return x
		case let .App(f, _):
			return f.functor
		}
	}

	var args : [Term] {
		func go(_ t : Term) -> [Term] {
			switch t {
			case .Var(_):
				return []
			case .Const(_):
				return []
			case let .App(f, x):
				return [x] + go(f)
			}
		}
		return go(self).reversed()
	}

	var funs : [Symbol] {
		switch self {
		case .Var(_):
			return []
		case let .Const(x):
			return [x]
		case let .App(f, x):
			return f.funs + x.funs
		}
	}

	var vars : [Symbol] {
		switch self {
		case let .Var(x):
			return [x]
		case .Const(_):
			return []
		case let .App(f, x):
			return f.funs + x.funs
		}
	}

	func mapVars(_ f : (Symbol) -> Symbol) -> Term {
		switch self {
		case let .Var(x):
			return .Var(f(x))
		case let .Const(x):
			return .Const(x)
		case let .App(t, u):
			return .App(t.mapVars(f), u.mapVars(f))
		}
	}

	func mapConsts(_ f : (Symbol) -> Symbol) -> Term {
		switch self {
		case let .Var(x):
			return .Var(x)
		case let .Const(x):
			return .Const(f(x))
		case let .App(t, u):
			return .App(t.mapConsts(f), u.mapConsts(f))
		}
	}
}

struct Expr<A> {
	let term : Term
	let arity : Int
	let eval : (Valuation<A>) -> A

	var upcast : Expr<Any> {
		fatalError()
	}
}

struct Atom<A> {
	let sym : Symbol
	let value : A

	var upcast : Atom<Any> {
		return Atom<Any>(sym: self.sym, value: self.value as Any)
	}
}

struct PGen<A> {
	let total : Gen<A>
	let partial : Gen<A>

	init(_ g : Gen<A>) {
		self.total = g
		self.partial = g
	}
}

typealias Strategy<A> = (Symbol, PGen<A>) -> Gen<A>

struct Variable<A> {
	let unVariable : Atom<PGen<A>>

	func map(_ f : (Symbol) -> Symbol) -> Variable<A> {
		let a2 = Atom(sym: f(self.unVariable.sym), value: self.unVariable.value)
		return Variable(unVariable: a2)
	}

	var upcast : Variable<Any> {
		fatalError()
	}
}

struct Constant<A> {
	let unConstant : Atom<A>

	func map(_ f : (Symbol) -> Symbol) -> Constant<A> {
		let a2 = Atom(sym: f(self.unConstant.sym), value: self.unConstant.value)
		return Constant(unConstant: a2)
	}

	var upcast : Constant<Any> {
		return Constant<Any>(unConstant: self.unConstant.upcast)
	}
}

struct Valuation<A> {
	let unValuation : (Variable<A>) -> A
}

protocol Signature {
	static var signature : Sig { get }
}

/// The left-biased `Maybe` `Monoid`
public struct First<A : Comparable> {
	public let value : () -> Optional<A>

	public init(_ x : @autoclosure @escaping () -> Optional<A>) {
		value = x
	}

	public static var mempty : First<A> {
		return First(nil)
	}

	public func op(_ other : First<A>) -> First<A> {
		if self.value() != nil {
			return self
		} else {
			return other
		}
	}
}

struct Sig : CustomStringConvertible {
	let constant : ConstantTypeRel
	let variables : VariableTypeRel
	let total : GenTypeMap
	let partial : GenTypeMap
//	let observers : ObserverTypeMap
//	let ords : ObserverTypeMap

	/*
	-- Witnesses for Typeable. The following types must have witnesses:
	--  * Any function argument.
	--  * Any function result.
	--  * Any partially-applied function type.
	--  * Any variable type.
	witnesses :: TypeMap Witnessed,
	*/

	// Depth of terms in the universe.
	let maxDepth_ : First<Int>

	// Size of terms in the universe.
	let maxSize_ : First<Int>

	// Minimum number of tests to run.
	let minTests_ : First<Int>

	// Maximum size parameter to pass to QuickCheck.
	let maxQuickCheckSize_ : First<Int>

	var maxDepth : Int {
		return self.maxDepth_.value() ?? 3
	}

	var maxSize : Int {
		return self.maxSize_.value() ?? 100
	}

	var minTests : Int {
		return self.minTests_.value() ?? 500
	}

	var maxQuickCheckSize : Int {
		return self.maxQuickCheckSize_.value() ?? 20
	}

	var description : String {
		return ""
	}
}

func quickSpec<A : Signature>(_ x : A) {
	return runTool({ sig in
		print("== Testing ==")
		genera
	}, x)
}


/*

-- | Run QuickSpec on a signature.
quickSpec :: Signature a => a -> IO ()
quickSpec = runTool $ \sig -> do
  putStrLn "== Testing =="
  r <- generate False (const partialGen) sig
  let clss = concatMap (some2 (map (Some . O) . classes)) (TypeMap.toList r)
      univ = concatMap (some2 (map (tagged term))) clss
      reps = map (some2 (tagged term . head)) clss
      eqs = equations clss
  printf "%d raw equations; %d terms in universe.\n\n"
    (length eqs)
    (length reps)

  let ctx = initial (maxDepth sig) (symbols sig) univ
      allEqs = map (some eraseEquation) eqs
      isBackground = all silent . eqnFuns
      keep eq = not (isBackground eq) || absurd eq
      absurd (t :=: u) = absurd1 t u || absurd1 u t
      absurd1 (Var x) t = x `notElem` vars t
      absurd1 _ _ = False
      (background, foreground) =
        partition isBackground allEqs
      pruned = filter keep
                 (prune ctx (filter (not . isUndefined) (map erase reps)) id
                   (background ++ foreground))
      eqnFuns (t :=: u) = funs t ++ funs u
      isGround (t :=: u) = null (vars t) && null (vars u)
      byTarget = innerZip [1 :: Int ..] (partitionBy target pruned)

  forM_ byTarget $ \eqs@((_,eq):_) -> do
    case target eq of
      NoTarget -> putStrLn "== Equations about several functions =="
      Target f -> printf "== Equations about %s ==\n" (show f)
    forM_ eqs $ \(i, eq) ->
      printf "%3d: %s\n" i (showEquation sig eq)
    putStrLn ""
promoteVal :: (forall a. Variable a -> Gen a) -> Gen Valuation
promoteVal g = do
  Capture eval <- capture
  return (Valuation (eval . g))

valuation :: Strategy -> Gen Valuation
valuation strat = promoteVal (\(Variable x) -> index (sym x) `variant` strat (sym x) (value x))

var :: Variable a -> Expr a
var v@(Variable (Atom x _)) = Expr (Var x) (symbolArity x) (\env -> unValuation env v)

con :: Constant a -> Expr a
con (Constant (Atom x v)) = Expr (Const x) (symbolArity x) (const v)

app :: Expr (a -> b) -> Expr a -> Expr b
app (Expr t a f) (Expr u _ x)
  | a == 0 = ERROR "oversaturated function"
  | otherwise = Expr (App t u) (a - 1) (\env -> f env (x env))
*/
