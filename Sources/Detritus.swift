//
//  Detritus.swift
//  SwiftCheck
//
//  Created by Robert Widmann on 9/22/16.
//  Copyright © 2016 Typelift. All rights reserved.
//

extension Dictionary {
	/// Returns a copy of the receiver with the given key associated with the
	/// given value.
	public func insert(_ k : Key, _ v : Value) -> [Key: Value] {
		var d = self
		d[k] = v
		return d
	}
}

extension Array {
	fileprivate func groupBy(_ p : (Element , Element) -> Bool) -> [[Element]] {
		var result = [[Element]]()
		var accumulator = [Element]()
		self.forEach { current in
			if let prev = accumulator.last {
				if p(prev, current) {
					accumulator.append(current)
				} else {
					result.append(accumulator)
					accumulator = [ current ]
				}
			} else {
				return accumulator.append(current)
			}
		}
		if !accumulator.isEmpty {
			result.append(accumulator);
		}
		return result
	}
}

func usort<A : Comparable>(_ xs : [A]) -> [A] {
	return xs.sorted().groupBy(==).map { $0.first! }
}
