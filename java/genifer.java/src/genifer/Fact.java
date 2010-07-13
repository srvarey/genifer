/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package genifer;

/**
  A Generic-Memory "Fact" item
 * @author SEH
 */
public class Fact extends Term {
    public final Term fact;

    /** truth value & confidence. default = (1.0, 1.0) */
    public final double truth;
    
    public  final double confidence;
    
    /** a list of clauses that this fact justifies */
    public  final Clause[] justifies;

    /** a list of clauses that justify this fact */
    public  final Clause[] justifiedBy;

    //TODO timestamp / time-marker?

    public Fact(Term fact, double truth, double confidence, Clause[] justifies, Clause[] justifiedBy) {
        super();
        this.fact = fact;
        this.truth = truth;
        this.confidence = confidence;
        this.justifies = justifies;
        this.justifiedBy = justifiedBy;

    }

}
