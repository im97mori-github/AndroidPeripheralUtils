package org.im97mori.ble.android.peripheral.utils;

import androidx.annotation.NonNull;
import androidx.lifecycle.Observer;

import java.util.function.Function;


public class MapObserver<T1, T2> implements Observer<T1> {

    private final Function<T1, T2> mFunction;
    private final Observer<T2> mMappedObserver;

    public MapObserver(@NonNull Function<T1, T2> function, @NonNull Observer<T2> mappedObserver) {
        mFunction = function;
        mMappedObserver = mappedObserver;
    }

    @Override
    public void onChanged(T1 t1) {
        mMappedObserver.onChanged(mFunction.apply(t1));
    }

}
