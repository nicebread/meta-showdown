<%@page import="java.util.*"%>

<%
String[][][] globals = {
{{"globals"}},
{{"txtstyles"},{"font-family","Arial"},{"font-size","16px"},{"font-weight","normal"},{"color","#000000"},{"background-color","#FFFFFF"}},
{{"btnstyles"},{"minsel","0"},{"maxsel","1"},{"cols","1"},{"flow","vertical"}}
  };


String[][][] ratings =
{
{{"ratings"},{"ncols","1"},{"flow","horizontal"},{"direction","left"}},
{{"rat1"},{"options","Continue"}}
};



String[][][] dem =
{
{{"dem"}, {"nelem","1"},{"musthave","0"},{"group","0"},{"gminmax","1.1"},{"gplace","0"},{"gorder","0"},{"gsequence","fixed"}, {"prefix","<center><a href='#' style='text-decoration:none;color:#000000' onclick='processresponse();'><b>Instructions</b></a></center><br>Most modern theories of decision making recognize the fact that decisions do not take place in a vacuum. Individual preferences and knowledge, along with situational variables can greatly impact the decision process. In order to facilitate our research on decision making we are interested in knowing certain factors about you, the decision maker. Specifically we are interested in whether you actually take the time to read the directions; if not, then some of our manipulations that rely on changes in the instructions will be ineffective. So, in order to demonstrate that you have read the instructions, please ignore the continue button below. Instead simply click on the title at the top of this screen (i.e., Instructions) to proceed to the next screen. Thank you very much.<br><br>"}},
{{"omdimc3"},{"choices","rat1"},{"stem",""}}
};



%>

<%!

public class index implements Comparable<index> {
  public int place=0;
  public int value=0;
  
  public int compareTo (index b) {
  
   if (value<b.value) return -1;
   if (value==b.value) return 0;
   if (value>b.value) return  1;
   
   return 0;
  }

}


public class item  {
  public int nitems=0;
  public ArrayList <String> flow = new ArrayList<String>();
  public ArrayList <String> ncols = new ArrayList<String>();
  public ArrayList <String> id = new ArrayList<String>();
  public ArrayList <String> prefix = new ArrayList<String>();
  public ArrayList <String> precheck = new ArrayList<String>();
  public ArrayList <String> stem = new ArrayList<String>();
  public ArrayList <String> bwidth = new ArrayList<String>();
  public ArrayList <String> minselect = new ArrayList<String>();
  public ArrayList <String> maxselect = new ArrayList<String>();
  public ArrayList <ArrayList<String>> values = new ArrayList<ArrayList<String>>();
  public ArrayList <ArrayList<String>> options = new ArrayList<ArrayList<String>>();
     
}

 void printmap (HashMap<String,ArrayList<String>> h) {
      System.out.println();
       for (String s: h.keySet()) 
         System.out.println(s+h.get(s).toString());
       System.out.println();
 
 }
 

  void  insertmapvalue (HashMap<String,ArrayList<String>> h, String key,String value) {
    ArrayList<String> t = new ArrayList<String> ();
    t.add(value);
    h.put(key,t);
   }
   
ArrayList<String> getkeysinstring (String s, char c) {

		ArrayList<String> as = new ArrayList<String>();
		int sindex=s.indexOf(c);
		int eindex=s.indexOf(c,sindex+1);
		while ((sindex >= 0) && (eindex>(sindex+1))) {
			as.add(s.substring(sindex+1,eindex));
			sindex=s.indexOf(c,eindex+1);
			if (sindex>0) eindex=s.indexOf(c,sindex+1);
		}
		
	//if (s.indexOf("@label")>=0) p(s+"-->"+as.toString());
	
	return as;
		

	}
	
	String joinstrings (ArrayList<String> as, String d) {
		String s= new String("");
		if ((as!=null) && (as.size()>0)) { 
		    for (int i=0;i<as.size()-1;i++) s+=as.get(i)+d;
            s+=as.get(as.size()-1); 
           }
		return s;
	}
	
	
	
	//tricky buggg.. needed to prepend the dot in the indexOf
	
	ArrayList<String> selectmapvalue (String tvk, ArrayList<String> listofkeys, HashMap<String,ArrayList<String>> mymap) {
	
	 ArrayList<String> t = new ArrayList<String> ();
	  
	 // non default search
	 for (String key:listofkeys) 
        if ( ((key.indexOf("."+tvk)+tvk.length()+1)==key.length()) && !(key.indexOf(".all.")>0)) 
          return mymap.get(key);
     
     // default search
     for (String key:listofkeys) 
        if ( ((key.indexOf("."+tvk)+tvk.length()+1)==key.length()) && (key.indexOf(".all.")>0)) 
          return mymap.get(key);
          
     // return nothing
     
     return t;
     
    
    }
    
                    

void p(String s) {
System.out.println(s);
}

void p(int s) {
System.out.println(s);
}


HashMap<String, ArrayList<String>> map2d (HashMap<String,ArrayList<String>> context, String[][][] structure) {
  return map2d(context,structure,null);
}

HashMap<String, ArrayList<String>> map2d (HashMap<String,ArrayList<String>> context, String[][][] structure, String selectedrow) {

		String ts="";
		String sep=".";
		String aprefix="";
		String iprefix="";
		String defprefix=context.get("selfid").get(0);
		int numrows=0;
		ArrayList <String> contextkeys = new ArrayList <String> ();
		
		
		HashMap<String,ArrayList<String>> contents = new HashMap<String, ArrayList<String>> ();  

        // this ensures that the arraylist values in contents can be modified without 
        // modifying the original context parameter.. however at the end, these context values are removed
        
        
        for ( String key : context.keySet()) 
         if (!key.equals("selfid"))
        {
             ArrayList<String> ta = new ArrayList<String>();
             for (String s:context.get(key)) ta.add(s);
             contents.put(key,ta);
             contextkeys.add(key);
         }
         
       
       insertmapvalue(contents,"selfid",structure[0][0][0]);
       
            
       for (int q=0;q<structure.length;q++)  {
       
            if (q==0) iprefix=defprefix; else iprefix=structure[0][0][0];
            
            if (q==0) aprefix=structure[0][0][0]+".all"; 
				else aprefix=structure[0][0][0]+"."+structure[q][0][0];
            
            
            ArrayList<String> tkeykey = getkeysinstring(aprefix,'@');
            
            
            for (String  tvk:tkeykey) {
    			String sconcat = joinstrings(selectmapvalue(tvk,contextkeys,contents),"");
                aprefix=aprefix.replace("@"+tvk+"@",sconcat); // gets rid of the prefix
			}
				
			
			if ((q==0) || (selectedrow==null) || ((selectedrow!=null) && selectedrow.equals(aprefix))) {          
            if (q>0) numrows++;
                       
            if (q>0) insertmapvalue(contents,iprefix+".rowid."+Integer.toString(numrows),aprefix);
            if (q>0) insertmapvalue(contents,iprefix+".all.numrows",Integer.toString(numrows));
            
			
                
        for (int k=1;k<structure[q].length;k++) {
             
             ArrayList<String> tkeys = getkeysinstring(structure[q][k][0],'@');
             ArrayList<String> tkeylist = new ArrayList<String>();
             
             
             if (tkeys.size()==0) tkeylist.add(structure[q][k][0]);
				else for (String key:tkeys) 
				 {
	              tkeylist.addAll(selectmapvalue(key,contextkeys,contents));
                  }
             ArrayList<String> tvaluelist = new ArrayList<String>();
          
              
             int first=0;int second=0;
			 for (int m=1;m<structure[q][k].length;m++) {
         
                ts=structure[q][k][m];
               
                ArrayList<String> tvaluekeys = getkeysinstring(ts,'@');
                ArrayList<String> tarr = new ArrayList<String>();
                
                first=ts.indexOf('@');
                if (first>=0) second=ts.indexOf('@',first+1);else second=-1;
                
           
                if ((first==0) && (second==ts.length()-1)) 
                {
                  tarr=selectmapvalue(tvaluekeys.get(0),contextkeys,contents);
                
                }
                 
                 else {
                 
                   first=ts.indexOf('['); second=ts.indexOf(']');
                   if ((first==0) && (second==ts.length()-1)) {
                   String st = ts.substring(1,ts.length()-1);
                   String[] rar = st.split("\\.\\.");
                   int start = Integer.parseInt(rar[0]);
                   int end = Integer.parseInt(rar[1]);
                   int nval=(int) Math.abs(start-end)+1; 
                   int cval=0; int count=0; count=start; 
                   while (cval<nval) {
					 tarr.add(new Integer(count).toString());
					 if (start<end) count++; else count--;
					 cval++;
                     
                   }
                 } 
                 
                  else
                 if (tvaluekeys.size()>0) {
                for (String  tvk:tvaluekeys) {
                    String sconcat = joinstrings(selectmapvalue(tvk,contextkeys,contents),""); 
                    ts=ts.replace("@"+tvk+"@",sconcat); // gets rid of the iprefix
				 }
				 
                	tarr.add(ts);  
				
                }
			} 
			
			 if  (tarr.size()==0) tarr.add(ts);
			 tvaluelist.addAll(tarr);
               
           } 
             
            
             
             //single key case
             if (tkeylist.size()==1) 
                contents.put(aprefix+sep+tkeylist.get(0),tvaluelist);
             
             //multiple key case
              else if ((tkeylist.size()>1) && (tvaluelist.size()==tkeylist.size())) 
                for (int i=0;i<tkeylist.size();i++) { 
                  ArrayList<String> ta = new ArrayList<String>();
                  ta.add(tvaluelist.get(i));
                 contents.put(aprefix+sep+tkeylist.get(i),ta);
               }          
	        }
	      }
	    
	    if (q==0)  // append to context keys
	    for (String key:contents.keySet()) 
	     if (key.indexOf(structure[0][0][0])>=0) contextkeys.add(key);
	    
	    }
	   
	   for ( String key : context.keySet()) 
         if (!key.equals("selfid"))
            contents.remove(key);
       
       
	   return contents;
	}

 
 
 
 
  int getint (HashMap<String,ArrayList<String>> h, String key) {
 
   return Integer.parseInt(h.get(key).get(0));
 
  }
  
  
  ArrayList<Integer> getintarray (HashMap<String,ArrayList<String>> h, String key) {
   
   ArrayList<Integer> a = new ArrayList <Integer> ();
   for (String s:h.get(key)) 
    a.add(Integer.parseInt(s));
    
   return a;
   
  }
  
  
  
  ArrayList <ArrayList <Integer>> get2ddotintarray (HashMap<String,ArrayList<String>> h, String key) {
 
    ArrayList <ArrayList <Integer>> s2darr = new ArrayList <ArrayList <Integer>> ();
    ArrayList <String> raw = h.get(key);
    for (String s: raw) {
    ArrayList <Integer> tarr = new ArrayList <Integer>();
    String[] sarr=s.split("\\.");
    for (int i=0;i<sarr.length;i++) tarr.add(Integer.parseInt(sarr[i]));
    s2darr.add(tarr);
    }
   return s2darr;
  } 
  


  void exitstatus(String status) {
   if (status.length()>0) {
    p(status);
    throw new RuntimeException(status);
   }
  }
 
  
 
 void initsequence (HashMap<String,ArrayList<String>> h,HashMap<String,ArrayList<String>> ratings, item myitem) {
 
  String status="";
  int minelem=0;
  int maxelem=0;
  int nchosensofar=0;
  
  Random generator = new Random();
  int k=generator.nextInt(10);
  for (int i=0; i<k; i++) {
    int j = generator.nextInt(10);
   }
   String mapid=h.get("selfid").get(0);
   
  // p(mapid); 
    
   //determine size of element array
   int nelem=getint(h,mapid+".all.nelem");
   if (nelem<=0) status+="there must be atleast one element\n"; exitstatus(status);
      
  // p(" nelem = "+Integer.toString(nelem));
   
   // determine musthaves
   ArrayList<Integer> musthave = getintarray(h,mapid+".all.musthave");
   // p(" musthave = "+musthave.toString()); 
  
   // determine groups 
   ArrayList <ArrayList<Integer>> group = get2ddotintarray(h,mapid+".all.group");
   if (group.get(0).get(0)==0) {
     ArrayList<Integer> tglist = new ArrayList<Integer> ();
     for (int i=0;i<nelem;i++) tglist.add(i+1);
     group.set(0,tglist);  
   }
   
   int ngroup = group.size();
   if (group.size()>nelem) status+="Number of groups cannot exceed nelem\n"; exitstatus(status);
   
   
   //p("group2d = "+group.toString());
   // groupindex, 1..nelem
   
    ArrayList<Integer> groupindex = new ArrayList<Integer> () ;
    groupindex.add(-1); //zero padding
    for (int i=1;i<=nelem;i++) groupindex.add(1);
    
    for (int i=0;i<ngroup;i++) {
     ArrayList<Integer> g = group.get(i);
     for (int j: g) 
      groupindex.set(j,i+1);  
    }
   
  // p("groupindex = "+groupindex.toString());
   
   // determine group minmaxes 
   ArrayList <ArrayList<Integer>> gminmax = get2ddotintarray(h,mapid+".all.gminmax");
   if (gminmax.size()!=ngroup) status+="length of gminmax should equal ngroup\n"; exitstatus(status);
   
   for (int i=0;i<ngroup;i++) {
    ArrayList<Integer> mm = gminmax.get(i);
    if (mm.size()!=2) status+="gminmax element should have 2 elements\n"; exitstatus(status);
    minelem=mm.get(0);maxelem=mm.get(1);
    if (minelem>maxelem) status+="minelem cant be greater than maxelem\n";exitstatus(status);
    if ((minelem<0) || (maxelem>group.get(i).size())) status+="gminmax range is invalid"; exitstatus(status);
   }
   
  // p ("gminmax = "+gminmax.toString());
   
   // determine gplace 
   ArrayList <ArrayList<Integer>> gplace = get2ddotintarray(h,mapid+".all.gplace");
 //  if (gplace.size()!=nelem) status+="length of gplace should equal nelem\n"; exitstatus(status);
  
    
   //p ("gplace = "+gplace.toString());
  
  // for (int i=0;i<ngroup;i++) {
   // ArrayList<Integer> pl = gplace.get(i);
    //for (int j:pl) 
     //if ((j<1) || (j>ngroup)) status+="gplace category id is invalid\n"; exitstatus(status);
   //}
   
   // determine gorder
   ArrayList<Integer> gorder = getintarray(h,mapid+".all.gorder");
   if (gorder.size()!=ngroup) status+="length of gorder should be ngroup\n"; exitstatus(status);
  
  // p ("gorder = "+gorder.toString());
  
  
   
   // determine gsequence
   ArrayList<String> gsequence = h.get(mapid+".all.gsequence");
   if (gsequence.size()!=ngroup) status+="length of gsequence should be ngroup\n"; exitstatus(status);
   
    //p ("gsequence = "+gsequence.toString());
  
   // initialize selected 1..nelem
   ArrayList<Integer> selected = new ArrayList<Integer> ();
   selected.add(-1); //zero padding 
   for (int i=1;i<=nelem;i++) selected.add(0); 
   
   //change the selected status of musthaves
   for (int i: musthave) 
    if (i>0) selected.set(i,1);   //tag the musthave elements as chosen using replace
   
    //p("selected = "+selected.toString());
  
   
   //make an optional which is the complement of musthave; this can have any number of elements
    ArrayList<Integer> optional = new ArrayList<Integer> ();
    for (int i=1;i<=nelem;i++) 
    if (selected.get(i)==0) optional.add(i);
    
    //p ("optional = "+optional.toString());
    
    ArrayList<Integer> nchosen = new ArrayList<Integer> ();
    nchosen.add(-1);
    for (int i=1;i<=ngroup;i++) nchosen.add(0);
    for (int i: musthave) 
     if (i>0) nchosen.set(groupindex.get(i),nchosen.get(groupindex.get(i))+1);
   
    //p ("nchosen = "+nchosen.toString());
    
    for (int i=1;i<=ngroup;i++)
     if (nchosen.get(i) > gminmax.get(i-1).get(0)) {status+="minimum is less than the musthave items for group";exitstatus(status);}
   
    
    //now select candidates for each group
    
    //lets shuffle each group
    
    for (ArrayList <Integer> a: group) Collections.shuffle(a);
    
     //p("group2d shuffled = "+group.toString());
    
    //pick elements from each group so that the total value is in the range minvalue to maxvalue
    
     for (int i=0;i<ngroup;i++) {
      ArrayList<Integer> g = group.get(i);
      nchosensofar=nchosen.get(i+1);
      minelem=gminmax.get(i).get(0);
      maxelem=gminmax.get(i).get(1);
      int ntarget=minelem+generator.nextInt(maxelem-minelem+1);
      //p("ntarget = "+ntarget+" chosensofar = "+nchosensofar);
      for (int j: g) { 
       int element= j;
       //p("element = "+element);
       if ((selected.get(element)==0) && nchosensofar<ntarget) {
        selected.set(element,1);
        nchosensofar++;
       }
      }
     }
   
    //p("selected after selection = "+selected.toString());
    
       
    // Now the sequencing begins
    
    //enforce gsequence for each group
    
    ArrayList<ArrayList<Integer>> sequence = new ArrayList<ArrayList<Integer>> ();
    for (int i=0;i<=ngroup;i++) sequence.add(new ArrayList<Integer>());
    
    for (int e=1;e<=nelem;e++) 
      if (selected.get(e)==1) 
		sequence.get(groupindex.get(e)).add(e);
     
    //p("sequence is = "+sequence.toString()); 
     //  p("gsequence is = "+gsequence.toString()); 
    
    //now apply gsequence directives
    
    for (int i=0; i<ngroup;i++) {
		if (gsequence.get(i).equals("random")) Collections.shuffle(sequence.get(i+1));
		if (gsequence.get(i).equals("reverse")) Collections.reverse(sequence.get(i+1));
    }
  //      p("sequence is = "+sequence.toString()); 

   // order directives.. sort nonzero groups.. then insert the zero groups randomly
   
   ArrayList<index> seqindex = new ArrayList<index> ();
   ArrayList<index> dontindex = new ArrayList<index> ();
   
   for (int i=0; i<ngroup; i++) {
   index tindex = new index();
    tindex.place=i;
    tindex.value=gorder.get(i); 
   if (gorder.get(i)!=0)  
     seqindex.add(tindex);
    else {
    tindex.value=0;
    dontindex.add(tindex);
    }
   }
  
  Collections.sort(seqindex);
  
  //now we add dontindex elements at random positions in seqindex
  
  for (index e:dontindex) {
    
    int position = generator.nextInt(seqindex.size()+1);
    seqindex.add(position,e);
  
  }
  
  
  //p("seqindex final");
  //for (index e: seqindex)
   //  System.out.print(e.place+" ");
  
  //p("");
  
  
  
  // so now we resequence in order..
  
  ArrayList <Integer> newsequence= new ArrayList <Integer> ();
  newsequence.add(-1);
  for (int i=0; i<ngroup; i++) {
    int g=seqindex.get(i).place;
    newsequence.addAll(sequence.get(g+1));
   }
   
  //p("newsequence = "+newsequence.toString());
  
  
  //finally use place.. the idea here is to preserve the
  //newsequence if it is consistent with the place
  //else look for something thats compatible
  //if not take what is available (instead of making exception)
  //and the results wont fully obey place constraints 
  
  int[] used = new int[nelem+1];
  used[0]=-1;
  for (int i=1;i<=nelem;i++) used[i]=-1;  // none are eligible
  
  for (int i=1;i<newsequence.size();i++) used[newsequence.get(i)]=0;  // these are eligible
  
  ArrayList <Integer> placesequence = new ArrayList <Integer> ();
  
  
  //p(gplace.toString());
  
  //p("");
 // p("groupindex="+groupindex.toString());
 // p("newsequence = "+newsequence.toString());
  
  // scope for simplifying the logic inside the loop
    
  //do this only if gplace size is more than 1.. so a 1-element 0 array will be ignored
  
  if (gplace.size()>1) {
  
       placesequence.add(-1);
 
  for (int i=1;(i<newsequence.size() && i<=gplace.size());i++) { //gplace has eligible categories for each spot
       boolean done=false;
       // check if current assignment is ok
        ArrayList<Integer> egroups = gplace.get(i-1);
       int current=groupindex.get(newsequence.get(i));
       //  p("used ="+used.toString());
       if ((egroups.contains(current) && used[newsequence.get(i)]==0)) {
         // p("i ="+i+"first loop egroups = "+egroups.toString()+"  current = "+current);
         used[newsequence.get(i)]=1;     
         placesequence.add(newsequence.get(i));
       } else
       //search in the newsequence to see if anything can be found for this spot
       {
         for (int s=1;s<newsequence.size();s++) {
          current=groupindex.get(newsequence.get(s));
          if ((egroups.contains(current) && used[newsequence.get(s)]==0)) {
        //  p("i = "+i+"second loop egroups = "+egroups.toString()+"  current = "+current);
          used[newsequence.get(s)]=1;     
          placesequence.add(newsequence.get(s));
          break; //nasty bug without break
        }
       } // s
      }  // else
     }  // for
    
   // if there are any selected items left out, they will be added at the end
   // this should happen only due to specification errors
   
         for (int s=1;s<newsequence.size();s++) 
          if (used[newsequence.get(s)]==0) {
          used[newsequence.get(s)]=1;     
          placesequence.add(newsequence.get(s));
       } // s
    
    }
    
    else
    
     {
        placesequence.addAll(newsequence);
     }
       
  // p("place sequence = "+placesequence.toString());
   
   ArrayList <String> ssequence = new ArrayList <String> ();
   
   for (int i=1;i<placesequence.size();i++) ssequence.add(placesequence.get(i).toString());
   
   //put things in map that will be actually used
   
   h.put(h.get("selfid").get(0)+".all.items",ssequence);
   
  String self=h.get("selfid").get(0);
 // p("self= "+self);
  
  
  if (myitem!=null) {
  
  myitem.nitems+=ssequence.size();
  
  for (int i=0;i<ssequence.size();i++) {
   
     String sid=h.get(self+".rowid."+ssequence.get(i)).get(0);
     
     String hid=h.get("selfid").get(0);
     
     myitem.id.add(sid);
     
     if (h.get(hid+".all.prefix")==null) myitem.prefix.add(""); else myitem.prefix.add(h.get(hid+".all.prefix").get(0));
     
     if (h.get(hid+".all.precheck")==null) myitem.precheck.add(""); else myitem.precheck.add(h.get(hid+".all.precheck").get(0));
     
     
     if (h.get(sid+".stem")!=null) myitem.stem.add(h.get(sid+".stem").get(0));
       else {
     
     p("sid= "+sid+"  hid= "+hid+"  i= "+i);   
    }
     
    // p(i+ " sid = "+sid);
     String rid ="";
     
     if (h.get(sid+".choices")!=null) rid=h.get(sid+".choices").get(0);else rid=h.get(self+".all.choices").get(0);
     
   // p(i+" rid = " +rid);
   
     // if (h.get(sid+".precheck")!=null) rid=h.get(sid+".precheck").get(0);else rid=h.get(self+".all.precheck").get(0);
     
     
     String minsel="1"; String maxsel="1";
     if (ratings.get("ratings.minsel")!=null) minsel= ratings.get("ratings.minsel").get(0);
     if (ratings.get("ratings.maxsel")!=null) maxsel= ratings.get("ratings.maxsel").get(0);
     
     if (ratings.get("ratings."+rid+".minsel")!=null) minsel= ratings.get("ratings."+rid+".minsel").get(0);
     if (ratings.get("ratings."+rid+".maxsel")!=null) maxsel= ratings.get("ratings."+rid+".maxsel").get(0);
        
     
     myitem.minselect.add(minsel);
     myitem.maxselect.add(maxsel);
     
     
     
     int noptions=ratings.get("ratings."+rid+".options").size();
     int max=0;
     
     for (int j=0;j<noptions;j++) 
       if (ratings.get("ratings."+rid+".options").get(j).length() > max) max=ratings.get("ratings."+rid+".options").get(j).length();
     
     myitem.bwidth.add(Integer.toString(max));
     
     // the key aspects are to reverse the options list and values list UNLESS the direction is left 
     
     boolean right=true;
     
     if (ratings.get("ratings.all.direction").get(0).equals("right")) right=true; else right=false;
     if (ratings.get("ratings."+rid+".direction")!=null) {
       if (ratings.get("ratings."+rid+".direction").get(0).equals("left")) right=false;else right=true;
     } 
    
    ArrayList <String> tolist = new ArrayList <String> ();
    tolist.addAll(ratings.get("ratings."+rid+".options")); 
     
     if (right) Collections.reverse(tolist); 
     myitem.options.add(tolist);
     
    ArrayList<String> tvals = new ArrayList<String> (); 
    for (int j=0;j<noptions;j++) 
     {
     if (right)
       tvals.add(Integer.toString(noptions-j));
     else tvals.add(Integer.toString(j+1));
    }
     
    
     myitem.values.add(tvals); 
     
     //reset flow and ncols if required
     
     String flow=ratings.get("ratings.all.flow").get(0);
     if (ratings.get("ratings."+rid+".flow")!=null) 
        flow=ratings.get("ratings."+rid+".flow").get(0);
     myitem.flow.add(flow);
     
     String ncols=ratings.get("ratings.all.ncols").get(0);
     if (ratings.get("ratings."+rid+".ncols")!=null) 
         ncols=ratings.get("ratings."+rid+".ncols").get(0);
     myitem.ncols.add(ncols);
     
     
    } 
  }
  
 }  
 
%>


<% 

//get the taskid to determine which domain is chosen



/*
StudySession studySession = (StudySession) session.getAttribute("studysession");
Node taskNode = (Node) studySession.getCurrentTask();
String taskid = "";
String ttaskid="";
if (taskNode instanceof PageTask) {
  taskid = taskNode.getId();
}



//String taskid="idi4";
//String ttaskid="";


  String number=taskid.substring(3,taskid.length());
  
  if (Integer.parseInt(number)>0)  idi[0][5][1]=number+"."+number;

*/  

	HashMap<String,ArrayList<String>> emptymap = new HashMap<String,ArrayList<String>> ();
	insertmapvalue(emptymap,"selfid","empty");
	
	HashMap<String,ArrayList<String>> ratingsmap = map2d(emptymap, ratings);
	HashMap<String,ArrayList<String>> demmap = map2d(emptymap, dem);
	
//	printmap(idimap);
	
//	p("");
	
//	p(idimap.get("idi.idi32.stem").get(0));
	
	item it = new item();
	
    initsequence(demmap,ratingsmap,it);
    
%>



<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" >
<head>
<script language="JavaScript" type="text/javascript" src="/implicit/common/en-us/js/task.js"></script>
    <meta http-equiv="msthemecompatible" content="no">
    <style type="text/css">
      .big {font-size: 22px;}
      .med {font-size: 17px;}
      .sml {font-size: 15px;}
    </style>
    <title></title>
    
<script language="JavaScript">

//window.moveTo(50,50);
// window.resizeTo(screen.width-100,screen.height-100);
 var xmlhttp=false;
 /*@cc_on @*/
 /*@if (@_jscript_version >= 5)
// JScript gives us Conditional compilation, we can cope with old IE versions.
// and security blocked creation of the objects.
 try {
  xmlhttp = new ActiveXObject("Msxml2.XMLHTTP");
 } catch (e) {
  try {
   xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
  } catch (E) {
   xmlhttp = false;
  }
 }
@end @*/
if (!xmlhttp && typeof XMLHttpRequest!='undefined') {
  xmlhttp = new XMLHttpRequest();
}


 function dummyjsp() {
    xmlhttp.open("GET", "/implicit/common/en-us/html/blank.jsp",true);
    xmlhttp.send("");
   }   
  

var spaces4 = '&nbsp;&nbsp;&nbsp;&nbsp;';
var spaces8 = '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;';
var spaces16 = '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;';


var begintaskrt=new Date().getTime();
  

 function Begin() {
    setInterval("dummyjsp()", 300000);
    mainitem.style.display='block';
    js_startrt[0] = new Date().getTime();
    mainitem.innerHTML=makeItem(0);
   }
   

</script>  


</head>

<body onLoad='Begin();'>

 <form method="post" action="/implicit/Study" name="form1" onSubmit="return assignformvalues();">
 <input type="hidden" name="mode" value="insQuesData">
 

<div id='mainitem' style='display:none'></div>

<div id='mybutton' style='display:none'>
<br /><br />
<center> 
<script language="JavaScript" type="text/javascript">writeButton("CONTINUE TO NEXT PAGE");</script> 
</center>
</div>

<%     
    for (int i=0;i<it.nitems;i++) 
     { String id=it.id.get(i);  id = id.substring(id.indexOf('.')+1,id.length());
  %>
  <input type="hidden" name="<%=id%>" value=".">
  <input type="hidden" name="<%=id%>rt" value="0">
  <input type="hidden" name="<%=id%>trt" value="0">
 <% } %> 

 
 <script language="JavaScript">

  var js_nitem = <%=it.nitems%>;
  var js_stem = new Array(js_nitem);
  var js_prefix = new Array(js_nitem);
  var js_precheck = new Array(js_nitem);
  
  
  //initialize js_stems to hold all the stems
  <% 
  
   
  
  for (int i=0;i<it.nitems;i++) { 
   String  prefix= it.prefix.get(i);
   String stem = it.stem.get(i);
   String precheck=it.precheck.get(i); 
         %>
     js_stem[<%=i%>]="<%=stem%>";
     js_prefix[<%=i%>]="<%=prefix%>";
     js_precheck[<%=i%>]="<%=precheck%>";
     
     
   <% } %>  
  
  //initialize js_options to hold all the options
  
  var js_options = new Array(js_nitem);
  var js_values = new Array(js_nitem);
  var js_nbuttons = new Array(js_nitem);
  var js_ncols = new Array(js_nitem);
  var js_flow = new Array(js_nitem);
  var js_bwidths = new Array(js_nitem);
  var js_minselect = new Array(js_nitem);
  var js_maxselect = new Array(js_nitem);
  var js_startrt = new Array(js_nitem);
  var js_endrt = new Array(js_nitem);
  var js_varnames = new Array(js_nitem);
  
  
  

  <%  for (int i=0;i<it.nitems;i++) { 
     
    %>
     js_options[<%=i%>]= new Array( <%=it.options.get(i).size()%> );
     js_values[<%=i%>]= new Array( <%=it.options.get(i).size()%> );
   
     
     js_nbuttons[<%=i%>]= <%=it.options.get(i).size()%>;
     js_varnames[<%=i%>]= "<%=it.id.get(i)%>";
    
     js_ncols[<%=i%>]= "<%=it.ncols.get(i)%>";
     js_flow[<%=i%>]= "<%=it.flow.get(i)%>";
     
     
     <% for (int j=0;j<it.options.get(i).size();j++) { %>
        js_options[<%=i%>][<%=j%>]="<%=it.options.get(i).get(j)%>";
      <% } %>  
     <% for (int j=0;j<it.options.get(i).size();j++) { %>
        js_values[<%=i%>][<%=j%>]="<%=it.values.get(i).get(j)%>";
      <% } %>  
     js_bwidths[<%=i%>]= 100 + ( <%=it.bwidth.get(i)%> - 8)*10;
     if (js_bwidths[<%=i%>] > 700) js_bwidths[<%=i%>] = 700;
     js_minselect[<%=i%>]=<%=it.minselect.get(i)%>;
     js_maxselect[<%=i%>]=<%=it.maxselect.get(i)%>;
        
   <% } %>  
   
  var js_selected = new Array(js_nitem);
  for (i=0;i<js_nitem;i++) js_selected[i]=-900;
 
  var js_multibuttons = new Array(101);
  for (i=0;i<101;i++) js_multibuttons[i]=0;
 
  
</script>



<script language="JavaScript">
  
  var mainitem = document.getElementById('mainitem');
  var mybutton = document.getElementById('mybutton');
  
  var gnext="";

 

function processresponse() {
  js_endrt[0]= new Date().getTime();
  js_selected[0]=0;
  assignformvalues ();
  document.form1.submit();
}

function assignformvalues () {
  
  <% for (int i=0;i<it.nitems;i++) {
  String id=it.id.get(i);
   id = id.substring(id.indexOf('.')+1,id.length());
   %>
  document.form1.<%=id%>rt.value = (js_endrt[<%=i%>] - js_startrt[<%=i%>]).toString();
  document.form1.<%=id%>trt.value = (js_endrt[<%=i%>] -  begintaskrt).toString();
  
  if (js_maxselect[<%=i%>]<=1) {
  if (js_selected[<%=i%>]!=-999) document.form1.<%=id%>.value = js_selected[<%=i%>].toString();
    else document.form1.<%=id%>.value = '.';
    } else {
    
    if (js_selected[<%=i%>]!="-999") document.form1.<%=id%>.value = js_selected[<%=i%>];
      else document.form1.<%=id%>.value = '.';
       
    }
 <% } %>
	
  return true;
}


// #BDEDFF light blue
   
 function makeItem(curitem) {
 
  
   
   
 
 // js_startrt[curitem]= new Date().getTime();
  
  var maxselect = js_maxselect[curitem];
 // var minselect = js_minselect[curitem];
  
  var icols=js_ncols[curitem];
  var nbuttons=js_nbuttons[curitem];
  var buttonwidth=js_bwidths[curitem];
  //var pctwidth=Math.floor(100/icols);
  var colwidth=buttonwidth+20;
  var prefix = '<button type="button" style="font-size:16; font-weight: bold; width:'+buttonwidth+'px; background-color:';
  var prefixskip = '<button type="button" style="font-size:16; font-weight: bold; width: 250 px; background-color:';
  var curdisplay='<table width="70%" align=center><tr><td>'+'<br><span  class=med>'+js_prefix[curitem]+'</span><span class=big style="background-color: #FFFFFF">'+js_stem[curitem]+'</span></td></tr></table> <table width="70%" align=center cellpadding="1">';
  
  
   
  
  var cols=0;
  
  curdisplay+='<tr>';
  for (var i=0; i < nbuttons; i++) {
      cols=cols+1;
      curdisplay+='<td class="med" width="'+colwidth+'px">'+prefix;
     
     // single select case
      if (maxselect==1) {
      if (js_values[curitem][i]==js_selected[curitem]) 
          curdisplay+='yellow"';
      else curdisplay+='#EEEEEE"';
     
      if (js_values[curitem][i]==js_selected[curitem])                             
          curdisplay+=' onclick=\'nextItem('+(curitem+1)+','+curitem+','+js_values[curitem][i]+')\'';
       else curdisplay+=' onclick=\'nextItem('+curitem+','+curitem+','+js_values[curitem][i]+')\'';
      
      curdisplay+='>'+js_options[curitem][i]+'</button></td>'; 
      } // maxselect is 1
      
     
      // multiple select case
      if (maxselect>1) {
      
      if (js_multibuttons[i]==1) 
          curdisplay+='#BDEDFF"';
      else curdisplay+='#EEEEEE"';
     
      curdisplay+=' onclick=\'nextItem('+curitem+','+curitem+','+i+')\'';
      
      curdisplay+='>'+js_options[curitem][i]+'</button></td>'; 
      } // maxselect is > 1
      
      
     
      if (cols==icols) {
       curdisplay+= '</tr>';
       if (i<nbuttons-1) curdisplay+='<tr>';
       cols=0;
      }
   }
   

  // the finished button
  
   if (maxselect>1) {
          curdisplay+='<tr><td class="med"><br/><br/>'+prefixskip;
  
          if (js_multibuttons[100]==1) 
              curdisplay+='yellow"';
            else curdisplay+='#EEEEEE"';
            
          if (js_multibuttons[100]==1)                             
              curdisplay+=' onclick=\'nextItem('+(curitem+1)+','+curitem+',100)\'';
		  else curdisplay+=' onclick=\'nextItem('+curitem+','+curitem+',100)\'';
          
          curdisplay+='>Finished and continue</button></td></tr>';
         
    curdisplay+='<tr><td colspan="'+icols+'">Select as many items as you wish, turning them <u>BLUE</u>.<br/>To remove an item from selection, click it once more.<br>When you are done select the &ldquo;Finished and continue&rdquo; button.<br/>Once the Finished button turns <u>YELLOW</u>, you can confirm by clicking it a second time.<br/><br/></td></tr>';
}
   
  
   if ((curitem<=1) && (maxselect<=1)) curdisplay+='<tr><td colspan="'+icols+'">Click once to color it yellow, then click again to continue.<br/><br/></td></tr>';

          
          /**
          curdisplay+='</table><table width="90%" align=right>';

          curdisplay+='<tr><td align="right" class="med"><br/><br/><br/><br/>'+prefixskip;
         
         if (maxselect<=1) {
          if (js_selected[curitem]==-999) 
              curdisplay+='yellow"';
            else curdisplay+='#EEEEEE"';
          if (js_selected[curitem]==-999)                             
              curdisplay+=' onclick=\'nextItem('+(curitem+1)+','+curitem+',-999)\'';
		  else curdisplay+=' onclick=\'nextItem('+curitem+','+curitem+',-999)\'';
          }
          
         
         if (maxselect>1) {
          if (js_multibuttons[99]==1) 
              curdisplay+='yellow"';
            else curdisplay+='#EEEEEE"';
          if (js_multibuttons[99]==1)                             
              curdisplay+=' onclick=\'nextItem('+(curitem+1)+','+curitem+',99)\'';
		  else curdisplay+=' onclick=\'nextItem('+curitem+','+curitem+',99)\'';
          }
          
        
          
          curdisplay+='>Decline to answer</button></td></tr>';
          
     **/
            
      
   curdisplay+='</table>';
   return(curdisplay);
 }
   

 function changestate(cur,fieldid,minlength,selfid) {
 
 /// alert(cur+" "+fieldid);
 
  typedsofar=document.getElementById(fieldid).value;
  
  if (typedsofar.length>=minlength) {
  
  document.getElementById(selfid).style.backgroundColor="yellow";
  js_values[cur][0] =  document.getElementById(fieldid).value;
  document.getElementById(selfid).onclick= function() {nextItem(cur+1,cur,js_values[cur][0]);};
 }
 
 }

 function nextItem(next,prev,button) {

   var maxselect = js_maxselect[prev];
   var nbuttons=   js_nbuttons[prev];
   
   if (maxselect<=1) js_selected[prev]=button;
    else {
     if (button>=0) js_multibuttons[button]= 1 - js_multibuttons[button]; // toggle state 
     if (button!=100) js_multibuttons[100]=0;
     if (button!=99) js_multibuttons[99]=0;
     
          
    }
  
   if (next!=js_nitem) {
   if (next!=prev) mainitem.innerHTML='';
   if (next!=prev) js_endrt[prev]= new Date().getTime();
   if (next!=prev) {
     
     //now we need to save
     
     if (maxselect>1) {
     sres="";
     for (i=0;i<nbuttons-1;i++)
      if (js_multibuttons[i]==1) 
       sres+=js_values[prev][i]+":";
       sres+=js_values[prev][nbuttons-1];
       
     if (sres=="") js_selected[prev]="-999";
      else js_selected[prev]=sres;
      //alert(js_selected[prev]);
      for (i=0;i<101;i++) js_multibuttons[i]=0;
     }
     
     js_startrt[next] = new Date().getTime();
   }
   gnext=next;
   if (next!=prev) setTimeout("mainitem.innerHTML=makeItem(gnext);",25);
    else mainitem.innerHTML=makeItem(gnext);
    }
    
    else {js_endrt[prev]= new Date().getTime();
    // window.location="/implicit/user/sriram/reimer/index.htm";
    setTimeout('mainitem.style.display="none";check=assignformvalues(); document.form1.submit(); ', 25);
      }     
   }

</script>


<script language="JavaScript">
var message="Context menu is disabled.";

function clickIE4(){
if (event.button==2){
alert(message);
return false;
}
}

function clickNS4(e){
if (document.layers||document.getElementById&&!document.all){
if (e.which==2||e.which==3){
alert(message);
return false;
}
}
}

if (document.layers){
document.captureEvents(Event.MOUSEDOWN);
document.onmousedown=clickNS4;
}
else if (document.all&&!document.getElementById){
document.onmousedown=clickIE4;
}

document.oncontextmenu=new Function("alert(message);return false")
</script>

</form>
</body>
</html>


</body>
</html>

 