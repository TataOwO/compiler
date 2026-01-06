
/* binary search trees */

class BST {
    int value;
    BST left;
    BST right;

    BST(BST g, int v, BST d) { value = v; left = g; right = d; }

    void insert(int x) {
	if (x == value) return;
	if (x < value) {
	    if (left == null)
		left = new BST(null, x, null);
	    else
		left.insert(x);
	} else
	    if (right == null)
		right = new BST(null, x, null);
	    else
		right.insert(x);
    }

    boolean contains(int x) {
	if (x == value) return true;
	if (x < value && left != null) return left.contains(x);
	if (right != null) return right.contains(x);
	return false;
    }

    String toStr() {
	String s = "";
	if (left != null) s = left.toStr();
	s = s + "(" + value + ")";
	if (right != null) s = s + right.toStr();
	return s;
    }

}

class Main {

    public static void main(String args[]) {

	BST dict = new BST(null, 1, null);
	dict.insert(17);
	dict.insert(5);
	dict.insert(8);
        System.out.print(dict.toStr() + "\n");

	if (dict.contains(5) &&
	    ! dict.contains(0) &&
	    dict.contains(17) &&
	    ! dict.contains(3) &&
	    dict.toStr().equals("(1)(5)(8)(17)"))
	    System.out.print("ok\n");
    }

}

