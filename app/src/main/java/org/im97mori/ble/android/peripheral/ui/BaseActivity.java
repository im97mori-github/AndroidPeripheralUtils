package org.im97mori.ble.android.peripheral.ui;

import android.text.Editable;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;

import io.reactivex.rxjava3.disposables.CompositeDisposable;

public abstract class BaseActivity extends AppCompatActivity {

//    protected ApplicationComponent mApplicationComponent;

    protected final CompositeDisposable mDisposable = new CompositeDisposable();

    @Override
    protected void onStop() {
        mDisposable.clear();
        super.onStop();
    }

    protected void distinctSetText(@NonNull EditText editText, @Nullable CharSequence text) {
        Editable currentText = editText.getText();
        if ((currentText == null && text != null)
                || (currentText != null && text == null)
                || (currentText != null && !currentText.toString().equals(text.toString()))) {
            editText.setText(text);
        }
    }

}
