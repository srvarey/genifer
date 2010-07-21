/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package genifer;

import genifer.swing.GeniferPanel;
import genifer.swing.util.SwingWindow;
import java.util.Arrays;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.Package;
import org.armedbear.lisp.Packages;

/**
 *
 * @author SEH
 */
public class GeniferLisp implements Genifer {

    public final Package defaultPackage;
    private final LispObject lispMemory;
    private final Interpreter lisp;
    public final Memory memory;

    public GeniferLisp(Memory memory) {
        super();

        this.memory = memory;
        
        lisp = Interpreter.createInstance();

        lisp.eval("(load \"lisp/main.lisp\")");
      
        lispMemory = lisp.eval("*generic-memory*");
               
        //System.out.println("packages: " + Arrays.asList(Packages.getAllPackages()));
        
        defaultPackage = Packages.findPackage("CL-USER");

    }

    public Memory getMemory() {
        return memory;
    }
    
    public LispObject eval(String e) {
        return lisp.eval(e);
    }
    
    public void abduce() {
    }

    public void induce() {
    }

    public void backwardChain() {
    }

    public void setDebug(int level) {
    }

    public void testSystem() {
    }

    public String toString() {
        return "[dump memory]";
    }

    protected void update() {
        
    }
    
    public LispObject[] getSymbols() {
        //System.out.println( defaultPackage.getAccessibleSymbols() );// findAccessibleSymbol("*generic-memory*");
        //System.out.println(genMem);

//        Object j = defaultPackage.getSymbols().
//        System.out.println(((Cons)defaultPackage.getSymbols().javaInstance()));
        return lispMemory.copyToArray();
        //return ((Cons)defaultPackage.getSymbols().javaInstance()).copyToArray();        
    }

//    public static void main(String[] args) {
//        new SwingWindow(new GeniferPanel(new GeniferLisp(new RAMMemory())), 800, 600, true);
//    }
    
}
