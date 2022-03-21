package org.im97mori.ble.android.peripheral.utils;


import android.os.Parcel;
import android.os.Parcelable;
import android.text.Editable;
import android.widget.AutoCompleteTextView;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

final class UtilsInner {

    private UtilsInner() {
    }

    static void setTextDistinct(@NonNull EditText editText, @Nullable CharSequence text) {
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

    @Nullable
    public static <T> T byteToParcelable(@Nullable byte[] data, @NonNull Parcelable.Creator<T> creator) {
        T result = null;
        if (data != null) {
            Parcel parcel = Parcel.obtain();
            parcel.unmarshall(data, 0, data.length);
            parcel.setDataPosition(0);
            result = creator.createFromParcel(parcel);
            parcel.recycle();
        }
        return result;
    }

    @NonNull
    public static byte[] parcelableToByteArray(@NonNull Parcelable parcelable) {
        Parcel parcel = Parcel.obtain();
        parcelable.writeToParcel(parcel, 0);
        int size = parcel.dataSize();
        parcel.setDataPosition(0);
        byte[] data = parcel.marshall();
        parcel.recycle();
        return data;
    }

}
