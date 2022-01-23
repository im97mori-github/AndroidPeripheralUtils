package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.content.Intent;

import androidx.annotation.NonNull;

import com.google.gson.Gson;

import org.im97mori.ble.DescriptorData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;

public abstract class BaseDescriptorSettingViewModel extends BaseSettingViewModel<Intent, Intent> {

    protected DescriptorData mDescriptorData;

    public BaseDescriptorSettingViewModel(@NonNull DeviceSettingRepository deviceSettingRepository, @NonNull Gson gson) {
        super(deviceSettingRepository, gson);
    }

}