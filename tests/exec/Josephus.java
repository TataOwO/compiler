
/*** doubly linked lists ***/

class DLL {

    int value;
    DLL next;
    DLL prev;

    DLL(int v)
    {
	value = v;
	next = prev = this;
    }

    void insertAfter(int v)
    {
	DLL e = new DLL(v);
	e.next = next;
	next = e;
	e.next.prev = e;
	e.prev = this;
    }

    void remove()
    {
	prev.next = next;
	next.prev = prev;
    }

    void display()
    {
	DLL c = this;
	System.out.print(c.value + " ");
	c = c.next;
	for ( ; c != this; )
        {
	    System.out.print(c.value + " ");
	    c = c.next;
        }
	System.out.print("\n");
    }

}

class Josephus {

    Josephus() {}

    /* circular list 1,2,...,n;
       returned element is that containing 1 */
    DLL cercle(int n)
    {
	DLL l = new DLL(1);
        int i;
	for (i = n; i >= 2; i=i-1)
	{
	    l.insertAfter(i);
	}
	return l;
    }

    int josephus(int n, int p)
    {
	/* c is current player */
	DLL c = cercle(n);

	/* as long as there is more than one player */
	for ( ; c != c.next; )
	{
	    /* eliminate a player */
            int i;
	    for (i = 1; i < p; i=i+1)	c = c.next;
	    c.remove();
	    System.out.print(c.value + " is eliminated\n");
	    c = c.next;
	}
	System.out.print("winner is " + c.value + "\n");
	return c.value;
    }

}

/*** Tests ***/

class Main
{

    public static void main(String args[])
    {
 	DLL l = new DLL(1);
 	l.display();
 	l.insertAfter( 3);
 	l.display();
 	l.insertAfter(2);
 	l.display();
 	l.next.remove();
 	l.display();

        Josephus j = new Josephus();
	DLL c = j.cercle(7);
	c.display();

	if (j.josephus(7, 5) == 6 &&
	    j.josephus(5, 5) == 2 &&
	    j.josephus(5, 17) == 4 &&
	    j.josephus(13, 2) == 11)
	    System.out.print("ok\n");
    }

}
