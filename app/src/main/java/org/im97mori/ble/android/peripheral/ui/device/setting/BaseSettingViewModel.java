package org.im97mori.ble.android.peripheral.ui.device.setting;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModel;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;

import io.reactivex.rxjava3.disposables.CompositeDisposable;

public abstract class BaseSettingViewModel extends ViewModel {

    protected final DeviceSettingRepository mDeviceSettingRepository;

    protected final Gson mGson;

    protected final CompositeDisposable mDisposable = new CompositeDisposable();

    public BaseSettingViewModel(@NonNull DeviceSettingRepository deviceSettingRepository, @NonNull Gson gson) {
        mDeviceSettingRepository = deviceSettingRepository;
        mGson = gson;
    }

    public void dispose() {
        mDisposable.clear();
    }

}