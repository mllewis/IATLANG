import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.random.EmpiricalDistribution;

/*
 * Util has the necessary functions for WEAT and WEFAT.
 * @author Aylin Caliskan (aylinc@princeton.edu)
 */

public class Utils {

public static double [] getCentroid(String [] words ,int wordDimension, String semanticModel, String delimiter, boolean caseSensitive){

double[] centroid = new double[wordDimension];
    
	//centroid of the embeddings of a list of words
    int counter=0;

    for(int i=0; i< words.length; i++){			
        double[] concept1Embedding = new double[wordDimension];
        concept1Embedding = Utils.getWordEmbedding(semanticModel, wordDimension, delimiter, words[i], caseSensitive);

        //If the word does not exist in the embeddings, make all the numeric values of the vector 999 so that you can exclude it from computations.
        if(concept1Embedding[1]!=999){
            counter++;
        }
    
        for(int column=0; column < wordDimension;column++){
            centroid[column]=centroid[column]+concept1Embedding[column];
        }
    }

    for(int column=0; column < wordDimension;column++){
        centroid[column]=centroid[column]/counter;
    }
    return centroid;
}
	
	
public static double cosineSimilarityOfVectors(String word1, String word2, boolean caseSensitive, String semanticModel, int wordDimension, String delimiter) {
	// Calculates cosine similarity given two words and parameters.

	double[] word1Embedding = getWordEmbedding(semanticModel, wordDimension, delimiter, word1, caseSensitive);
	double[] word2Embedding = getWordEmbedding(semanticModel, wordDimension, delimiter, word2, caseSensitive);
	return cosineSimilarity(word1Embedding, word2Embedding);		
}

public static double cosineSimilarity(double[] docVector1, double[] docVector2) {
	// Calculates cosine similarity of two vectors.
    double dotProduct = 0.0;
    double magnitude1 = 0.0;
    double magnitude2 = 0.0;
    double cosineSimilarity = 0.0;

    for (int i = 0; i < docVector1.length; i++) //docVector1 and docVector2 must be of same length
    {
        dotProduct += docVector1[i] * docVector2[i];  //a.b
        magnitude1 += Math.pow(docVector1[i], 2);  //(a^2)
        magnitude2 += Math.pow(docVector2[i], 2); //(b^2)
    }

    magnitude1 = Math.sqrt(magnitude1);//sqrt(a^2)
    magnitude2 = Math.sqrt(magnitude2);//sqrt(b^2)

    if (magnitude1 != 0.0 | magnitude2 != 0.0) {
        cosineSimilarity = dotProduct / (magnitude1 * magnitude2);
    } else {
        return 0.0;
    }
    
    return cosineSimilarity;
}


public static String[] removeCategoryWordsIfNotInDictionary(String[] category,String semanticModel, int wordDimension,String delimiter, boolean caseSensitive){
	//Removes a word from a list if it does not exist in the word embeddings.

	System.out.println("Array before check is:"+Arrays.toString(category));
	System.out.println("Array length before check is:"+category.length);

	//check if embedding exists and if not remove it
	for(int i=category.length-1; i>=0 ; i--){
		double[] concept1Embedding = new double[wordDimension];
		concept1Embedding = getWordEmbedding(semanticModel, wordDimension, delimiter, category[i], caseSensitive);
		if(concept1Embedding[0]==999){
			category = ArrayUtils.removeElement(category, category[i]);}
	}
	System.out.println("Array after check is:"+Arrays.toString(category));
	System.out.println("Array length after check is:"+category.length);
	
	
	return category;
}


public static double[] getWordEmbedding(String semanticModel, int wordDimension, String delimiter,  String word, boolean caseSensitive) {
    // Get the numeric vector of a word according to the parameters.
	double[] array=new double[wordDimension];
    boolean breakReader = true;
    String vector="";
    String line;
    String attribute = word; 	    	   
	
    try {
        BufferedReader br = new BufferedReader(new FileReader(semanticModel));
        
        try {
            while (((line = br.readLine()) != null) && breakReader==true) {
                
                if (caseSensitive==false) {
                    attribute=attribute.toLowerCase();
                    line = line.toLowerCase();
                }
                
                if (line.length() >= attribute.length() + 1) {
                    if (line.substring(0, attribute.length() + 1).equals(attribute + delimiter)) {

                        vector= line;	 
                        java.util.List<String> dimensions = Arrays.asList(vector.split(delimiter));
                        
                        for (int column = 1; column < dimensions.size(); column++) {
                            //array is the word embedding
                            double d = Double.parseDouble(dimensions.get(column));
                            array[column-1]=array[column-1]+d;
                        }
                        
                        breakReader = false;		    	    
                    }
                }
            }
            
            if (breakReader == true){
                System.out.println(word +" not in model.");
            }
        } catch (NumberFormatException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        try {
            br.close();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
		}

	} catch (FileNotFoundException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}

	if (breakReader == true){
		   for(int column=0; column < wordDimension;column++){
		    	//word does not exist in dictionary
		    	array[column]=999;
		    }
        return array;
    }
	
	
    return array; 
	
}

public static int getWordFrequencyOrder(String semanticModel, int wordDimension, String delimiter,  String word, boolean caseSensitive) {
    double[] array=new double[wordDimension];
    boolean breakReader = true;
    String vector="";
    String line;
    String attribute = word; 
	int counter=0;
    try {
        BufferedReader br = new BufferedReader(new FileReader(semanticModel));

        try {
            while (((line = br.readLine()) != null) && breakReader==true){
                counter++;
                
                if(caseSensitive==false){
                    attribute=attribute.toLowerCase();
                }
                
                if(line.startsWith(attribute + delimiter)){
                    vector= line;	 
                    java.util.List<String> dimensions = Arrays.asList(vector.split(delimiter));
                    
                    for(int column=1; column < dimensions.size();column++){
                        //array is the word embedding
                        double d = Double.parseDouble(dimensions.get(column));
                        array[column-1]=array[column-1]+d;
                    }
                    
                    breakReader = false;		    	    
                }
            }
            
            if (breakReader == true){
                counter=0;
                System.out.println(word +" not in model.");
            }
        } catch (NumberFormatException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        try {
            br.close();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        } catch (FileNotFoundException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
        }

        if (breakReader == true){
           for(int column=0; column < wordDimension;column++){
                //word does not exist in dictionary
                counter=0;
            }
        }


        return counter; 

        }

        public static double mean(double[] arr) {
        double sum = 0;
        for (int i = 0; i < arr.length; i++) {
        sum += arr[i];
    }
    return sum / arr.length;
}

public static double findDeviation(double[] arr) {
	Arrays.sort(arr);
	double mean = mean(arr);
	double squareSum = 0;

	for (int i = 0; i < arr.length; i++) {
		squareSum += Math.pow(arr[i] - mean, 2);
	}
	
	return Math.sqrt((squareSum) / (arr.length - 1));
}


public static double effectSize( double[] arr, double mean) {
	
	double effect = (mean)/(findDeviation(arr));		
	return effect;	
	
}


public static float calculateWomenPercentage( String csvFile, String profession) throws IOException {
	// Calculate percentage of profession made of women, when given a properly-formatted file
	// of BLS statistics
	
	float percentage=0;
	BufferedReader br = new BufferedReader(new FileReader(csvFile));
	float totalCounter=0;
	float womenCounter=0;
	String line;
	String[] entry;
	while (((line = br.readLine()) != null)){
		if(line.contains(profession)){
			entry = line.split(",");
			totalCounter = totalCounter + Float.parseFloat(entry[0]);
			womenCounter = womenCounter + (Float.parseFloat(entry[0])*Float.parseFloat(entry[1]));			
		}
	}
	br.close();		
	percentage = womenCounter/totalCounter;

	return percentage/100;		
}


public static float calculateWomenNamePercentage( String csvFile, String name) throws IOException {
	// Calculate percentage of people with a certain name who are women, calculated from
	// US Census Data
	float percentage=0;
	BufferedReader br = new BufferedReader(new FileReader(csvFile));
	String line;
	String[] entry;
	while (((line = br.readLine()) != null)){
		line = line.toLowerCase();
		if(line.contains(name)){
			entry = line.split(",");
			percentage = (Float.parseFloat(entry[2])*Float.parseFloat(entry[3]))/
					((Float.parseFloat(entry[2])*Float.parseFloat(entry[3])) + (Float.parseFloat(entry[2])*Float.parseFloat(entry[4])));
		}
	}
	br.close();		
	return percentage;		
}

public static double calculateCumulativeProbability(double[] arr, double value, String distribution) {
	
	double cumulative=-100;
	if(distribution.equals("empirical")){
		Arrays.sort(arr);
		
		EmpiricalDistribution dist = new EmpiricalDistribution(arr.length);
	    dist.load(arr);	    
	    cumulative = dist.cumulativeProbability(value);
	}
	
	if(distribution.equals("normal")){
		Arrays.sort(arr);
		NormalDistribution dist = new NormalDistribution(mean(arr), findDeviation(arr));	
		cumulative = dist.cumulativeProbability(value);
	}
	
	return cumulative;

}

public static void writeFile(String allLines,String fileName, boolean append)
	{
		File aFile = new File(fileName);
		FileWriter aFileWriter;
		try {
		 	    if(aFile.exists() == false)
		 	    		aFile.createNewFile();
				
				aFileWriter = new FileWriter(aFile, append); // Open in Append mode
				
				{
					aFileWriter.write(allLines);
				}
				aFileWriter.close();
			 	   
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
 	   
	}

public static double getTestStatistic(String[] concept1, String[] concept2, String[] stereotype1, String[] stereotype2, boolean caseSensitive, String semanticModel, int wordDimension, String delimiter) {
		double differenceOfMeans =0;
		double differenceOfMeansConcept1 =0;
		double differenceOfMeansConcept2 =0;
		
		//concept1 to stereotype1 and stereotype2
		for(int i=0; i< concept1.length; i++){
			double[] concept1Embedding = new double[wordDimension];
			concept1Embedding = getWordEmbedding(semanticModel, wordDimension, delimiter, concept1[i], caseSensitive);
			
			double meanConcept1Stereotype1=0;
			for(int j=0; j< stereotype1.length; j++){
				double[] stereotype1Embedding = new double[wordDimension];
				stereotype1Embedding = getWordEmbedding(semanticModel, wordDimension, delimiter, stereotype1[j], caseSensitive);
				double similarity = cosineSimilarity(concept1Embedding, stereotype1Embedding);
				meanConcept1Stereotype1 = meanConcept1Stereotype1 + similarity;
			}	
			meanConcept1Stereotype1 = meanConcept1Stereotype1/stereotype1.length;
			
			double meanConcept1Stereotype2=0;
			for(int j=0; j< stereotype2.length; j++){
				double[] stereotype2Embedding = new double[wordDimension];
				stereotype2Embedding = getWordEmbedding(semanticModel, wordDimension, delimiter, stereotype2[j], caseSensitive);
				double similarity = cosineSimilarity(concept1Embedding, stereotype2Embedding);
				meanConcept1Stereotype2 = meanConcept1Stereotype2 + similarity;
			}	
			meanConcept1Stereotype2 = meanConcept1Stereotype2/stereotype2.length;
			differenceOfMeansConcept1 = differenceOfMeansConcept1 + meanConcept1Stereotype1 - meanConcept1Stereotype2;				
		}
		
		differenceOfMeansConcept1 = differenceOfMeansConcept1/(concept1.length);

		
		//concept2 to stereotype1 and stereotype2
        for(int i=0; i< concept2.length; i++){
            double[] concept2Embedding = new double[wordDimension];
            concept2Embedding = getWordEmbedding(semanticModel, wordDimension, delimiter, concept2[i], caseSensitive);
            
            double meanConcept2Stereotype1=0;
            for(int j=0; j< stereotype1.length; j++){
                double[] stereotype1Embedding = new double[wordDimension];
                stereotype1Embedding = getWordEmbedding(semanticModel, wordDimension, delimiter, stereotype1[j], caseSensitive);
                double similarity = cosineSimilarity(concept2Embedding, stereotype1Embedding);
                meanConcept2Stereotype1 = meanConcept2Stereotype1 + similarity;
            }	
            meanConcept2Stereotype1 = meanConcept2Stereotype1/stereotype1.length;
            
            double meanConcept2Stereotype2=0;
            for(int j=0; j< stereotype2.length; j++){
                double[] stereotype2Embedding = new double[wordDimension];
                stereotype2Embedding = getWordEmbedding(semanticModel, wordDimension, delimiter, stereotype2[j], caseSensitive);
                double similarity = cosineSimilarity(concept2Embedding, stereotype2Embedding);
                meanConcept2Stereotype2 = meanConcept2Stereotype2 + similarity;
            }	
            meanConcept2Stereotype2 = meanConcept2Stereotype2/stereotype2.length;	
            differenceOfMeansConcept2 = differenceOfMeansConcept2 + meanConcept2Stereotype1 - meanConcept2Stereotype2;				
        }				

		differenceOfMeansConcept2 = differenceOfMeansConcept2/(concept2.length);

		differenceOfMeans= differenceOfMeansConcept1 - differenceOfMeansConcept2;		
		System.out.println("The differenceOfMeans is: "+differenceOfMeans);

        return differenceOfMeans;		
}
	
	
public static double [] getEntireDistribution(String[] concept1, String[] concept2, String[] stereotype1, String[] stereotype2, boolean caseSensitive, String semanticModel, int wordDimension, String delimiter, int iterations) {

    String[] bothConcepts = (String[])ArrayUtils.addAll(concept1, concept2);
  	double[] distribution = new double[bothConcepts.length];
  	System.out.println("Getting the entire distribution...");

		for(int i=0; i< bothConcepts.length; i++){
			double[] conceptEmbedding = new double[wordDimension];
			conceptEmbedding = getWordEmbedding(semanticModel, wordDimension, delimiter, bothConcepts[i], caseSensitive);
			double similarityToStereotype1=0; 	
			double similarityToStereotype2=0; 			


			for(int j=0; j<stereotype1.length;j++){		
				double[] stereotype1Embedding = new double[wordDimension];
				stereotype1Embedding = getWordEmbedding(semanticModel, wordDimension, delimiter, stereotype1[j], caseSensitive);
				similarityToStereotype1 = similarityToStereotype1 + cosineSimilarity(conceptEmbedding, stereotype1Embedding);
				}		
			similarityToStereotype1 = similarityToStereotype1/stereotype1.length;
		
			for(int j=0; j<stereotype2.length;j++){		
				double[] stereotype2Embedding = new double[wordDimension];
				stereotype2Embedding = getWordEmbedding(semanticModel, wordDimension, delimiter, stereotype2[j], caseSensitive);
				similarityToStereotype2 = similarityToStereotype2 + cosineSimilarity(conceptEmbedding, stereotype2Embedding);
				}		
			
			similarityToStereotype2 = similarityToStereotype2/stereotype2.length;			
	    	distribution[i] = similarityToStereotype1 - similarityToStereotype2;				
		}
	
		return distribution;
	}
	
	
	
public static double [] nullDistribution(String[] concept1, String[] concept2, String[] stereotype1, String[] stereotype2, boolean caseSensitive, String semanticModel, int wordDimension, String delimiter, int iterations) {
		
		//permute concepts and for each permutation calculate getTestStatistic and save it in your distribution 		
		String[] bothConcepts = (String[])ArrayUtils.addAll(concept1, concept2);
		System.out.println("Generating null distribution...");
		double [][] stereotype1NullMatrix= new double[stereotype1.length][bothConcepts.length];
		double [][] stereotype2NullMatrix= new double[stereotype2.length][bothConcepts.length];

		for(int i=0; i< stereotype1.length; i++){
			double[] stereotype1Embedding = new double[wordDimension];
			stereotype1Embedding = getWordEmbedding(semanticModel, wordDimension, delimiter, stereotype1[i], caseSensitive);
			for(int j=0; j<bothConcepts.length;j++){		
				double similarity; 			
				double[] nullEmbedding = new double[wordDimension];
				nullEmbedding = getWordEmbedding(semanticModel, wordDimension, delimiter, bothConcepts[j], caseSensitive);
				similarity = cosineSimilarity(nullEmbedding, stereotype1Embedding);
				stereotype1NullMatrix[i][j]=similarity;
				}		
		}
		
		for(int i=0; i< stereotype2.length; i++){
			double[] stereotype2Embedding = new double[wordDimension];
			stereotype2Embedding = getWordEmbedding(semanticModel, wordDimension, delimiter, stereotype2[i], caseSensitive);
			for(int j=0; j<bothConcepts.length;j++){		
				double similarity; 			
				double[] nullEmbedding = new double[wordDimension];
				nullEmbedding = getWordEmbedding(semanticModel, wordDimension, delimiter, bothConcepts[j], caseSensitive);
				similarity = cosineSimilarity(nullEmbedding, stereotype2Embedding);
				stereotype2NullMatrix[i][j]=similarity;
				}		
		}
	    
		//assuming that both concepts have the same number of elements
		int setSize = bothConcepts.length/2;
		System.out.println("Number of permutations:" + iterations);
  	    double[] distribution = new double[iterations];
		
		Integer[] toShuffle = new Integer[bothConcepts.length];
  	    for(int i=0; i < (setSize*2); i++ ){
  		    toShuffle[i]=i;
  	    }
		
		for(int iter=0; iter<iterations;iter++){
			Collections.shuffle(Arrays.asList(toShuffle));
			
			//calculate mean for each null shuffle
			double meanSimilaritycon1str1=0;
			double meanSimilaritycon1str2=0;
			double meanSimilaritycon2str1=0;
			double meanSimilaritycon2str2=0;

			
			for(int i=0; i< stereotype1.length; i++){
				for(int j=0; j< setSize; j++){				
					meanSimilaritycon1str1 = meanSimilaritycon1str1+ stereotype1NullMatrix[i][toShuffle[j]];				
				}				
			}
			
			for(int i=0; i< stereotype2.length; i++){
				for(int j=0; j< setSize; j++){				
					meanSimilaritycon1str2 = meanSimilaritycon1str2+ stereotype2NullMatrix[i][toShuffle[j]];				
				}				
			}
			
			for(int i=0; i< stereotype1.length; i++){
				for(int j=0; j< setSize; j++){				
					meanSimilaritycon2str1 = meanSimilaritycon2str1 + stereotype1NullMatrix[i][toShuffle[j+setSize]];				
				}				
			}
			
			for(int i=0; i< stereotype2.length; i++){
				for(int j=0; j< setSize; j++){				
					meanSimilaritycon2str2 = meanSimilaritycon2str2 + stereotype2NullMatrix[i][toShuffle[j+setSize]];				
				}				
			}
			
		meanSimilaritycon1str1 = meanSimilaritycon1str1/((stereotype1.length)*setSize);
		meanSimilaritycon1str2 = meanSimilaritycon1str2/((stereotype2.length)*setSize);
		meanSimilaritycon2str1 = meanSimilaritycon2str1/((stereotype1.length)*setSize);
		meanSimilaritycon2str2 = meanSimilaritycon2str2/((stereotype2.length)*setSize);

		distribution[iter] = (meanSimilaritycon1str1 - meanSimilaritycon1str2) - meanSimilaritycon2str1 + meanSimilaritycon2str2;
		}
		return distribution;
	}
	
	
public static double [] getPValueAndEffect(String[] concept1, String[] concept2, String[] stereotype1, String[] stereotype2, boolean caseSensitive, String semanticModel, int wordDimension, String delimiter, String distribution, int iterations) {
		double []pValue= new double[3];
		double testStatistic = getTestStatistic(concept1, concept2, stereotype1, stereotype2,  caseSensitive,  semanticModel,  wordDimension,  delimiter); 
		double [] nullDistribution = nullDistribution(concept1, concept2, stereotype1, stereotype2,  caseSensitive,  semanticModel,  wordDimension,  delimiter, iterations); 
		double [] entireDistribution = getEntireDistribution(concept1, concept2, stereotype1, stereotype2,  caseSensitive,  semanticModel,  wordDimension,  delimiter, iterations); 
		
		pValue[0] = 1-calculateCumulativeProbability(nullDistribution,  testStatistic, distribution);
		pValue[1] = effectSize(entireDistribution, testStatistic);
		pValue[2] = findDeviation(nullDistribution);
		return pValue;	
}	

public ArrayList<ArrayList<Integer>> permute(int[] num) {
		ArrayList<ArrayList<Integer>> result = new ArrayList<ArrayList<Integer>>();
	 
		//start from an empty list
		result.add(new ArrayList<Integer>());
	 
		for (int i = 0; i < num.length; i++) {
			//list of list in current iteration of the array num
			ArrayList<ArrayList<Integer>> current = new ArrayList<ArrayList<Integer>>();
	 
			for (ArrayList<Integer> l : result) {
				// # of locations to insert is largest index + 1
				for (int j = 0; j < l.size()+1; j++) {
					// + add num[i] to different locations
					l.add(j, num[i]);
	 
					ArrayList<Integer> temp = new ArrayList<Integer>(l);
					current.add(temp);
	 
					// - remove num[i] add
					l.remove(j);
				}
			}
	 
			result = new ArrayList<ArrayList<Integer>>(current);
		}
	 
		return result;
}
	
	

	
	
public static String readFile(String fileName) throws IOException {
	    BufferedReader br = new BufferedReader(new FileReader(fileName));
	    try {
	        StringBuilder sb = new StringBuilder();
	        
	        String line = br.readLine();
	
	        while (line != null) {
	            sb.append(line);
	            sb.append("\n");
	            line = br.readLine();
	        }
	        return sb.toString();
	    } finally {
	        br.close();
	    }
}



public static List<String> readFileUTF8(File file, boolean readAll) {
    	List<String> allWords = new ArrayList<String>();
    		
		BufferedReader reader = null;
	    
 	    if(file.exists() == true)
			try {
				reader = new BufferedReader(new InputStreamReader(new FileInputStream(file),"UTF8"));
				
				String dataLine = reader.readLine();
				
				while(dataLine!=null)
				{
					if(readAll)
						allWords.add(dataLine);
					else{
						if(!allWords.contains(dataLine))
						{
							
							allWords.add(dataLine);
						}
					}
					
					dataLine = reader.readLine();
				}
				
				reader.close();
			}catch(IOException e)
			{
				e.printStackTrace();
			}
			
			return allWords;
    	
        }
}


