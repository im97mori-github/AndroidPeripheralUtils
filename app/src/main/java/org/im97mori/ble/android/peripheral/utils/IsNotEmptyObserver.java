package org.im97mori.ble.android.peripheral.utils;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.lifecycle.Observer;

public class IsNotEmptyObserver extends MapObserver<String, Boolean> {

    public IsNotEmptyObserver(@NonNull Observer<Boolean> observer) {
        super(t -> !TextUtils.isEmpty(t), observer);
    }

}
