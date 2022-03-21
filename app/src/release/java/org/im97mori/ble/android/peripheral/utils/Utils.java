package org.im97mori.ble.android.peripheral.utils;


import android.os.Parcel;
import android.os.Parcelable;
import android.text.Editable;
import android.widget.AutoCompleteTextView;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public final class Utils {

    private Utils() {
    }

    public static void setTextDistinct(@NonNull EditText editText, @Nullable CharSequence text) {
        UtilsInner.setTextDistinct(editText, text);
    }

    public static void stackLog(Object... args) {
    }

    @Nullable
    public static <T> T byteToParcelable(@Nullable byte[] data, @NonNull Parcelable.Creator<T> creator) {
        return UtilsInner.byteToParcelable(data, creator);
    }

    @NonNull
    public static byte[] parcelableToByteArray(@NonNull Parcelable parcelable) {
        return UtilsInner.parcelableToByteArray(parcelable);
    }

}
