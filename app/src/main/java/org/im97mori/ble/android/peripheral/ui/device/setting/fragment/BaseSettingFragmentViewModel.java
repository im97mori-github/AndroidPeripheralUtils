package org.im97mori.ble.android.peripheral.ui.device.setting.fragment;

import androidx.annotation.NonNull;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseSettingViewModel;

public abstract class BaseSettingFragmentViewModel extends BaseSettingViewModel {

    public BaseSettingFragmentViewModel(@NonNull DeviceSettingRepository deviceSettingRepository, @NonNull Gson gson) {
        super(deviceSettingRepository, gson);
    }

    abstract public String getModuleDataString();

}