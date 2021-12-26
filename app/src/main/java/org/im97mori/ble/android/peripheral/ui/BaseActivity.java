package org.im97mori.ble.android.peripheral.ui;

import androidx.appcompat.app.AppCompatActivity;

import io.reactivex.rxjava3.disposables.CompositeDisposable;

public abstract class BaseActivity extends AppCompatActivity {

    protected final CompositeDisposable mDisposable = new CompositeDisposable();

    @Override
    protected void onStop() {
        mDisposable.dispose();
        super.onStop();
    }

}
