package SortArr;

import java.util.AbstractList;
import java.util.ArrayList;

public class SortArr<E> {

    private boolean lt(Integer elem1, Integer elem2) {
	if (elem1 != null && elem2 != null)
	    return elem1.intValue() < elem2.intValue();
	else if (elem1 == null) 
	    return true;
	else 
	    return false;
    }
	
    private void insert(Integer n, AbstractList<Integer> l) {
	boolean done = false;
	
	for (int i=0; !done && i<l.size(); i++) {
	    if (lt(n,l.get(i))) {
		l.add(i,n);
		done=true;
	    }
	}
	
	if (done) l.add(n);
    }
    
    public AbstractList<Integer> insertionSort(AbstractList<Integer> l) {
	AbstractList<Integer> result = null;
	
	if(l != null) {
	    result = new ArrayList<Integer>();
		
	    for (int i=0; i<l.size(); i++)
		insert(l.get(i),result);
	    }

	return result;
    }
}
	
