package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.content.Intent;

import androidx.annotation.NonNull;

import com.google.gson.Gson;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;

public abstract class BaseCharacteristicViewModel extends BaseSettingViewModel<Intent, Intent> {

    protected CharacteristicData mCharacteristicData;

    public BaseCharacteristicViewModel(@NonNull DeviceSettingRepository deviceSettingRepository, @NonNull Gson gson) {
        super(deviceSettingRepository, gson);
    }

}