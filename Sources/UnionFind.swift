//
//  UnionFind.swift
//  SwiftCheck
//
//  Created by Robert Widmann on 9/22/16.
//  Copyright © 2016 Typelift. All rights reserved.
//

struct UFS {
	let links : Dictionary<Int, Int>
	let sym : Int

	init(_ links : Dictionary<Int, Int>, _ sym : Int) {
		self.links = links
		self.sym = sym
	}

	init(_ n : Int) {
		self.links = [:]
		self.sym = n
	}
}

typealias UF<A> = State<UFS, A>

//data Replacement = Int :> Int

func modifyLinks(_ f : @escaping (Dictionary<Int, Int>) -> Dictionary<Int, Int>) -> UF<()> {
	return modify { s in
		return UFS(f(s.links), s.sym)
	}
}

func modifySym(_ f : @escaping (Int) -> Int) -> UF<()> {
	return modify { s in
		return UFS(s.links, f(s.sym))
	}
}



//putLinks l = modifyLinks (const l)

func newSym() -> UF<Int> {
	return get().bind { s in
		return modifySym({ x in x + 1 }).bind { _ in
			return UF.pure(s.sym)
		}
	}
}


//(=:=) :: Int -> Int -> UF (Maybe Replacement)
//s =:= t | s == t = return Nothing
//s =:= t = do
//rs <- rep s
//rt <- rep t
//if (rs /= rt) then do
//modifyLinks (IntMap.insert rs rt)
//return (Just (rs :> rt))
//else return Nothing

func rep(_ t : Int) -> UF<Int> {
	return get().fmap({ $0.links }).bind { m in
		switch m[t] {
		case .none:
			return UF.pure(t)
		case let .some(t2):
			return rep(t2).bind { r in
				return when(t2 != r, modifyLinks({ d in d.insert(t, r) })).bind { _ in
					return UF<Int>.pure(r)
				}
			}
		}
	}
}

private func when(_ p : Bool, _ s : UF<()>) -> UF<()> {
	if p {
		return s
	}
	return UF<()>.pure(())
}

