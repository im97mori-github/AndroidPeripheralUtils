package org.im97mori.ble.android.peripheral.utils;


import android.text.Editable;
import android.widget.AutoCompleteTextView;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

public final class Utils {

    private Utils() {
    }

    private static Gson mGson;

    @NonNull
    public synchronized static Gson createGsonInstance() {
        if (mGson == null) {
            mGson = new GsonBuilder()
                    .create();
        }
        return mGson;
    }

    public static void setTextDistinct(@NonNull EditText editText, @Nullable CharSequence text) {
        Editable currentText = editText.getText();
        if ((currentText == null && text != null)
                || (currentText != null && text == null)
                || (currentText != null && !currentText.toString().equals(text.toString()))) {
            if (editText instanceof AutoCompleteTextView) {
                ((AutoCompleteTextView) editText).setText(text, false);
            } else {
                editText.setText(text);
            }
        }
    }

    public static void stackLog(Object... args) {
    }

}
