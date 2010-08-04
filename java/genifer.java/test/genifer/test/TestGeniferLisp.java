/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package genifer.test;

import genifer.GeniferLisp;
import genifer.SimpleMemory;
import java.util.Arrays;
import java.util.List;
import junit.framework.TestCase;
import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.StandardObject;
import org.armedbear.lisp.*;

/**
 *
 * @author seh
 */
public class TestGeniferLisp extends TestCase {
    private static Object defaultPackage;

    public void testMemory() {
        GeniferLisp gl = new GeniferLisp(new SimpleMemory());
        
        List<LispObject> objects = Arrays.asList(gl.getSymbols());
        for (LispObject lo : objects) {
            StandardObject so = (StandardObject)lo;
            System.out.println(lo.getClass() + ": " + lo.writeToString() + " " + lo.typeOf().writeToString());
            String lispType = lo.typeOf().writeToString();
            if (lispType.equals("RULE-ITEM")) {
                
            } else if (lispType.equals("FACT-ITEM")) {
                //        (format t "**** [~a] fact: ~a ~%" (id item) (fact item))
                //        (setf tv (tv item))
                //        (format t "  TV:           ~a ~%" (car tv))
                //        (format t "  confidence:   ~a ~%" (cdr tv))
                //        (format t "  justifies:    ~a ~%" (justifies    item))
                //        (format t "  justified-by: ~a ~%" (justified-by item)))                
                
                System.out.println("(id " + lo.writeToString() + ")");
                System.out.println(so.getInstanceSlotValue(Lisp.intern("ID", gl.defaultPackage)).javaInstance());
                System.out.println(so.getInstanceSlotValue(Lisp.intern("FACT", gl.defaultPackage)).javaInstance());
                System.out.println(so.getInstanceSlotValue(Lisp.intern("TV", gl.defaultPackage)).car().javaInstance());
                System.out.println(so.getInstanceSlotValue(Lisp.intern("TV", gl.defaultPackage)).cdr().javaInstance());
                System.out.println(so.getInstanceSlotValue(Lisp.intern("JUSTIFIES", gl.defaultPackage)).cdr().javaInstance());
                System.out.println(so.getInstanceSlotValue(Lisp.intern("JUSTIFIED-BY", gl.defaultPackage)).cdr().javaInstance());                                
            }

        }

        try {
            Symbol voidsym =
                    gl.defaultPackage.findAccessibleSymbol("VOID-FUNCTION");
            Function voidFunction = (Function) voidsym.getSymbolFunction();
            voidFunction.execute(new JavaObject(new SimpleMemory()));
        } catch (Throwable t) {
            System.out.println("exception!");
            t.printStackTrace();
        }

    }

    public static void main(String[] args) {
        new TestGeniferLisp().testMemory();

     }
}
