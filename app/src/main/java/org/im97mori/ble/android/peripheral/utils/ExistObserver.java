package org.im97mori.ble.android.peripheral.utils;

import androidx.annotation.NonNull;
import androidx.lifecycle.Observer;

import java.util.Objects;

public class ExistObserver extends MapObserver<String, Boolean> {

    public ExistObserver(@NonNull Observer<Boolean> observer) {
        super(t -> !Objects.isNull(t), observer);
    }

}
