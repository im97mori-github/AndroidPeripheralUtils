package org.im97mori.ble.android.peripheral.ui.device.setting;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModel;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;

public abstract class BaseSettingViewModel extends ViewModel {

    protected final DeviceRepository mDeviceRepository;
    protected final Gson mGson;

    public BaseSettingViewModel(@NonNull DeviceRepository deviceRepository, @NonNull Gson gson) {
        mDeviceRepository = deviceRepository;
        mGson = gson;
    }
}