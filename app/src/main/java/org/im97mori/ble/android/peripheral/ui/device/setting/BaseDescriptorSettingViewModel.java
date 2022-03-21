package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.content.Intent;

import androidx.annotation.NonNull;

import org.im97mori.ble.DescriptorData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;

public abstract class BaseDescriptorSettingViewModel extends BaseSettingViewModel<Intent, Intent> {

    protected DescriptorData mDescriptorData;

    public BaseDescriptorSettingViewModel(@NonNull DeviceSettingRepository deviceSettingRepository) {
        super(deviceSettingRepository);
    }

}