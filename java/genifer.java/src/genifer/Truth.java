/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package genifer;

/**
 *
 * @author seh
 */
public interface Truth {
    
    public double getProbability();    
    public double getConfidence();
    
    public static class SimpleTruth implements Truth {
        public double prob;                   
        public double confidence;
        
        public SimpleTruth(double p, double confidence) {
            super();
            this.prob = p;
            this.confidence = confidence;
        }

        @Override
        public String toString() {
            return prob + ", " + confidence;
        }

        @Override
        public double getConfidence() {
            return confidence;
        }

        @Override
        public double getProbability() {
            return prob;
        }
        
        
    }

    
}
