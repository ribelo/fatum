package ribelo.fatum;

import clojure.lang.*;

public class Fail extends RuntimeException implements IExceptionInfo, ILookup, Associative {
    public final IPersistentMap data;

    public Fail(String msg, IPersistentMap data) {
        super(msg, null, false, false);
        if (data != null) {
            this.data = data;
        } else {
            throw new IllegalArgumentException("Additional data must be non-nil.");
        }
    }

    public IPersistentMap getData() {
        return data;
    }

    public String toString () {
        return "Fail: " + getMessage() + " " + data.toString();
    }

    public Object valAt(Object key) {
        return data.valAt(key, null);
    }

    public Object valAt(Object key, Object notFound) {
        return data.valAt(key, notFound);
    }

    public boolean containsKey(Object key) {
        return data.containsKey(key);
    }

    public IMapEntry entryAt(Object key) {
        return data.entryAt(key);
    }

    public Associative assoc(Object key, Object val) {
        return new Fail(super.getMessage(), data.assoc(key, val));
    }

    public int count() {
        return data.count();
    }

    public IPersistentCollection cons(Object o) {
        return data.cons(o);
    }

    public boolean equiv(Object o) {
        if (o instanceof Fail) {
           if (super.getMessage() == ((Fail)o).getMessage() && data.equiv(((Fail)o).getData())) {
               return true;
           } else {
               return false;
           }
        } else {
            return false;
        }
    }

    public IPersistentCollection empty() {
        return new Fail(super.getMessage(), PersistentArrayMap.EMPTY);
    }

    public ISeq seq() {
        return data.seq();
    }
}
