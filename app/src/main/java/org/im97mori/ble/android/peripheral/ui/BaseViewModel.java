package org.im97mori.ble.android.peripheral.ui;

import androidx.lifecycle.ViewModel;

import io.reactivex.rxjava3.disposables.CompositeDisposable;

public abstract class BaseViewModel extends ViewModel {

    protected final CompositeDisposable mDisposable = new CompositeDisposable();

    public void dispose() {
        mDisposable.clear();
    }

}