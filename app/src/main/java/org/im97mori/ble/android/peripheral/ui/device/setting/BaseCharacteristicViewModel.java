package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.content.Intent;

import androidx.annotation.NonNull;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;

public abstract class BaseCharacteristicViewModel extends BaseSettingViewModel<Intent, Intent> {

    protected CharacteristicData mCharacteristicData;

    public BaseCharacteristicViewModel(@NonNull DeviceSettingRepository deviceSettingRepository) {
        super(deviceSettingRepository);
    }

}