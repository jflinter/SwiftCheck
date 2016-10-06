//
//  CongruenceClosure.swift
//  SwiftCheck
//
//  Created by Robert Widmann on 9/22/16.
//  Copyright © 2016 Typelift. All rights reserved.
//

struct CCS {
	let funUse : Dictionary<Int, [(Int, Int)]>
	let argUse : Dictionary<Int, [(Int, Int)]>
	let lookup : Dictionary<Int, Dictionary<Int, Int>>
	let uf : UFS
}

typealias CC<A> = State<CCS, A>

infix operator %%

func crep(_ s : Int) -> CC<Int> {
	fatalError()
	// return liftUF (UF.rep s)
}

func =?=(t : Int, u : Int) -> CC<Bool> {
	fatalError()
}

func =%=(t : Int, u : Int) -> CC<Bool> {
	fatalError()
}

func %%(_ f : Int, _ x : Int) -> CC<Int> {
	fatalError()
//	return gets { m in
//		return crep(f).bind { f2 in
//			return crep(x).bind { x2 in
//				switch m[x2].flatMap({ d in d[f2] }) {
//				case .none:
////					return newSym().bind { c in
////
////					}
//					fatalError()
//				case let .some(k):
//					return CC<Int>.pure(k)
//				}
//			}
//		}
//	}
}
