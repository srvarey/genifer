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
//;;;     w            = size of support (ie, total number of times the rule is involved in proofs)
//;;;     e+           = positive examples
//;;;     e-           = negative examples
//;;;     ancestors    = a list of ancestor rules of this rule
//;;;     ancestors-to = a list of rules that this rule is ancestor to
//(defclass rule-item () (
//  (head        :initarg :head        :accessor head)
//  (body        :initarg :body        :accessor body)
//  (id          :initarg :id          :accessor id)
//  (w           :initarg :w           :accessor w            :initform 100)
//  (e+          :initarg :e+          :accessor e+           :initform nil)
//  (e-          :initarg :e-          :accessor e-           :initform nil)
//  (ancestors   :initarg :ancestors   :accessor ancestors    :initform nil)
//  (ancestor-to :initarg :ancestor-to :accessor ancestor-to  :initform nil)
//))
    private double support;
    public final List<Term> examplePlus = new LinkedList();
    public final List<Term> exampleNeg = new LinkedList();
    public final Term head;
    public final Term body;
    public final List<Rule> parents = new LinkedList();
    public final List<Rule> children = new LinkedList();

    public Rule(Term head, Term body, double support) {
        this.support = support;
        this.head = head;
        this.body = body;
    }

    public double getSupport() {
        return support;
    }

    public void setSupport(double support) {
        this.support = support;
    }        
    
}

