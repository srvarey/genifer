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
    public final Term term;

    public final Truth truth;
        
    /** a list of clauses that this fact justifies */
    //public  final Clause[] justifies;

    /** a list of clauses that justify this fact */
    //public  final Clause[] justifiedBy;

    //TODO timestamp / time-marker?

    public Fact(Term term, double truth, double confidence /*, Clause[] justifies, Clause[] justifiedBy*/) {
        super();
        this.term = term;
        this.truth = new Truth(truth, confidence);
        //this.justifies = justifies;
        //this.justifiedBy = justifiedBy;

    }

}
