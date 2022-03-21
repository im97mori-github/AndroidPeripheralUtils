package org.im97mori.ble.android.peripheral.ui.device.setting.fragment;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseSettingViewModel;

public abstract class BaseSettingFragmentViewModel extends BaseSettingViewModel<byte[], byte[]> {

    public BaseSettingFragmentViewModel(@NonNull DeviceSettingRepository deviceSettingRepository) {
        super(deviceSettingRepository);
    }

}