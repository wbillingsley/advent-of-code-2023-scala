package util

// It seems useful to keep some range intersection code lying around
// (though I won't worry about cleaning it up for now)

type LongRange = scala.collection.immutable.NumericRange[Long]

// Divide a range at a particular value, putting the value into the right hand side
def bisect(r:Range, n:Int):(Range, Range) = 
    (r.start until n) -> (n until r.end)


extension [T] (r1:LongRange) {
    
    /** Finds the overlapping and non-overlapping part of this range with another */
    def clip(r2:LongRange):(LongRange, LongRange, LongRange) = {
        val overlapStart = Math.max(r1.start, r2.start)
        val overlapEnd = Math.min(r1.end, r2.end)

        (
            Range.Long(r1.start, overlapStart, 1), // untransformed "before" fragment
            Range.Long(overlapStart, overlapEnd, 1), // overlap
            Range.Long(overlapEnd, r1.end, 1) // untransformed "after" fragment
        )
    }

    /** Whether any of this range intersects with another */
    def clips(r2:LongRange) = 
        Math.max(r1.start, r2.start) < Math.min(r1.end, r2.end)

    def shifted(delta:Long) = 
        Range.Long(r1.start + delta, r1.end + delta, 1)

}
