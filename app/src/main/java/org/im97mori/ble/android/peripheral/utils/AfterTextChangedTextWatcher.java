package org.im97mori.ble.android.peripheral.utils;

import android.text.Editable;
import android.text.TextWatcher;

import androidx.annotation.NonNull;
import androidx.lifecycle.Observer;


public class AfterTextChangedTextWatcher implements TextWatcher {

    private final Observer<String> mObserver;

    public AfterTextChangedTextWatcher(@NonNull Observer<String> observer) {
        mObserver = observer;
    }

    @Override
    public void beforeTextChanged(CharSequence s, int start, int count, int after) {

    }

    @Override
    public void onTextChanged(CharSequence s, int start, int before, int count) {

    }

    @Override
    public void afterTextChanged(Editable s) {
        mObserver.onChanged(s.toString());
    }
}
