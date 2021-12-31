package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.content.Intent;

import androidx.annotation.NonNull;

import com.google.gson.Gson;

import org.im97mori.ble.DescriptorData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;

import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;

public abstract class BaseDescriptorSettingViewModel extends BaseSettingViewModel {

    protected DescriptorData mDescriptorData;

    public BaseDescriptorSettingViewModel(@NonNull DeviceSettingRepository deviceSettingRepository, @NonNull Gson gson) {
        super(deviceSettingRepository, gson);
    }

    public abstract void observeSetup(@NonNull Intent intent
            , @NonNull Action onComplete
            , @NonNull Consumer<? super Throwable> onError);

    public abstract void observeSave(@NonNull Consumer<Intent> onSuccess
            , @NonNull Consumer<? super Throwable> onError);

}