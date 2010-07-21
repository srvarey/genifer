/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package genifer;

/**
 * A Genifer instance
 * @author seh
 */
public interface Genifer {
    
    public void abduce();
    public void induce();
    public void backwardChain();
    public void setDebug(int level);

    public Memory getMemory();
    
}
