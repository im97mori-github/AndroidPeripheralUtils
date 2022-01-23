package org.im97mori.ble.android.peripheral.ui.device.setting;

import androidx.annotation.NonNull;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.Observer;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.ui.BaseViewModel;

import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;

public abstract class BaseSettingViewModel<T1, T2> extends BaseViewModel {

    protected static final String KEY_SAVED_DATA = "KEY_SAVED_DATA";

    protected final DeviceSettingRepository mDeviceSettingRepository;

    protected final Gson mGson;

    public BaseSettingViewModel(@NonNull DeviceSettingRepository deviceSettingRepository, @NonNull Gson gson) {
        mDeviceSettingRepository = deviceSettingRepository;
        mGson = gson;
    }

    public abstract void observeSetup(@NonNull T1 t1
            , @NonNull Action onComplete
            , @NonNull Consumer<? super Throwable> onError);

    public abstract void observeSavedData(@NonNull LifecycleOwner owner, @NonNull Observer<T2> observer);

    public abstract void save(@NonNull Consumer<? super Throwable> onError);
}