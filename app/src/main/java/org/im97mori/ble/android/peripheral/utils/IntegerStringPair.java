package org.im97mori.ble.android.peripheral.utils;

import androidx.annotation.NonNull;
import androidx.core.util.Pair;

public class IntegerStringPair extends Pair<Integer, String> {

    /**
     * Constructor for a Pair.
     *
     * @param first  the first object in the Pair
     * @param second the second object in the pair
     */
    public IntegerStringPair(Integer first, String second) {
        super(first, second);
    }

    @NonNull
    @Override
    public String toString() {
        return second;
    }

}
