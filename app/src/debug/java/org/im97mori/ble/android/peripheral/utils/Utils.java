package org.im97mori.ble.android.peripheral.utils;


import android.os.Parcelable;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.im97mori.stacklog.LogUtils;

public final class Utils {

    private Utils() {
    }

    public static void setTextDistinct(@NonNull EditText editText, @Nullable CharSequence text) {
        UtilsInner.setTextDistinct(editText, text);
    }

    public static void stackLog(Object... args) {
        LogUtils.stackLogWithOffset(2, args);
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
