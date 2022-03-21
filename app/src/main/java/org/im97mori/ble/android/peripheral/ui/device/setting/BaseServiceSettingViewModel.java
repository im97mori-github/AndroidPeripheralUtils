package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.content.Intent;

import androidx.annotation.NonNull;

import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;

public abstract class BaseServiceSettingViewModel extends BaseSettingViewModel<Intent, Intent> {

    protected ServiceData mServiceData;

    public BaseServiceSettingViewModel(@NonNull DeviceSettingRepository deviceSettingRepository) {
        super(deviceSettingRepository);
    }

}