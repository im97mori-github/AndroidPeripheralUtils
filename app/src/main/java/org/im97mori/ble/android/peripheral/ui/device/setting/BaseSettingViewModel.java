package org.im97mori.ble.android.peripheral.ui.device.setting;

import androidx.annotation.NonNull;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.ui.BaseViewModel;

public abstract class BaseSettingViewModel extends BaseViewModel {

    protected final DeviceSettingRepository mDeviceSettingRepository;

    protected final Gson mGson;

    public BaseSettingViewModel(@NonNull DeviceSettingRepository deviceSettingRepository, @NonNull Gson gson) {
        mDeviceSettingRepository = deviceSettingRepository;
        mGson = gson;
    }

}