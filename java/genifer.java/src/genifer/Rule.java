/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package genifer;

import java.util.LinkedList;
import java.util.List;

/**
  A Generic-Memory "Rule" item
 * @author SEH
*/
public class Rule {
//;;; Entries:
//;;;     id
//;;;     w            = size of support (ie, total number of times the rule is involved in proofs)
//;;;     e+           = positive examples
//;;;     e-           = negative examples
//;;;     ancestors    = a list of ancestor rules of this rule
//;;;     ancestors-to = a list of rules that this rule is ancestor to

    private double support;
    public final List<Formula> ePositive = new LinkedList();
    public final List<Formula> eNegative = new LinkedList();
    
    public final Formula formula;

    public final List<Rule> parents = new LinkedList();
    public final List<Rule> children = new LinkedList();

    public Rule(Formula formula, double support) {
        this.support = support;
        this.formula = formula;
    }

    public double getSupport() {
        return support;
    }

    public void setSupport(double support) {
        this.support = support;
    }        

}
