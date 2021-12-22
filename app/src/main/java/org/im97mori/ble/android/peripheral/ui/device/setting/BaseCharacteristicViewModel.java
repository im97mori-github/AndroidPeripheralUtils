package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.content.Intent;

import androidx.annotation.NonNull;

import com.google.gson.Gson;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;

public abstract class BaseCharacteristicViewModel extends BaseSettingViewModel {

    public BaseCharacteristicViewModel(@NonNull DeviceRepository deviceRepository, @NonNull Gson gson) {
        super(deviceRepository, gson);
    }

    protected CharacteristicData mCharacteristicData;

    @NonNull
    public abstract Completable setup(@NonNull Intent intent);

    @NonNull
    public abstract Single<Intent> save();

}