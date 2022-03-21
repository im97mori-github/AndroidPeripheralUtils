package org.im97mori.ble.android.peripheral.hilt.repository;

import static android.bluetooth.BluetoothAdapter.STATE_OFF;
import static android.bluetooth.BluetoothAdapter.STATE_ON;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;
import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;

import android.bluetooth.BluetoothAdapter;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;

import androidx.annotation.NonNull;

import org.im97mori.ble.BLEServerCallback;
import org.im97mori.ble.BLEUtilsAndroid;
import org.im97mori.ble.MockData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.im97mori.ble.android.peripheral.utils.Utils;
import org.im97mori.ble.profile.blp.peripheral.BloodPressureProfileMockCallback;
import org.im97mori.ble.profile.peripheral.AbstractProfileMockCallback;
import org.im97mori.ble.service.bls.peripheral.BloodPressureServiceMockCallback;
import org.im97mori.ble.service.dis.peripheral.DeviceInformationServiceMockCallback;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;

public class BluetoothSettingRepository {

    private final Context mApplicationContext;

    private final Set<Consumer<Boolean>> mBluetoothStatusConsumerList = Collections.synchronizedSet(new LinkedHashSet<>());

    @Inject
    public BluetoothSettingRepository(@NonNull @ApplicationContext Context context) {
        mApplicationContext = context.getApplicationContext();
        mApplicationContext.registerReceiver(new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                boolean isOn = intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_ON;
                mBluetoothStatusConsumerList
                        .stream()
                        .sequential()
                        .forEach(booleanConsumer -> booleanConsumer.accept(isOn));
            }
        }, new IntentFilter(BluetoothAdapter.ACTION_STATE_CHANGED));
    }

    public void addBluetoothStatusConsumer(@NonNull Consumer<Boolean> consumer) {
        mBluetoothStatusConsumerList.add(consumer);
    }

    public void removeBluetoothStatusConsumer(@NonNull Consumer<Boolean> consumer) {
        mBluetoothStatusConsumerList.remove(consumer);
    }

    public boolean isBluetoothEnabled() {
        return BLEUtilsAndroid.isBluetoothEnabled(mApplicationContext);
    }

    public void bluetoothEnable() {
        BLEUtilsAndroid.bluetoothEnable(mApplicationContext);
    }

    public void bluetoothDisable() {
        BLEUtilsAndroid.bluetoothDisable(mApplicationContext);
    }

    @NonNull
    public AbstractProfileMockCallback createProfileMockCallback(@NonNull DeviceSetting deviceSetting
            , @NonNull BLEServerCallback... callbacks) {
        if (DEVICE_TYPE_BLOOD_PRESSURE_PROFILE == deviceSetting.getDeviceType()) {
            ServiceData blsServiceData = null;
            ServiceData disServiceData = null;
            MockData mockData = Utils.byteToParcelable(deviceSetting.getDeviceSettingData(), MockData.CREATOR);

            if (mockData != null) {
                for (ServiceData serviceData : mockData.serviceDataList) {
                    if (BLOOD_PRESSURE_SERVICE.equals(serviceData.uuid)) {
                        blsServiceData = serviceData;
                    } else if (DEVICE_INFORMATION_SERVICE.equals(serviceData.uuid)) {
                        disServiceData = serviceData;
                    }
                }
            }
            return new BloodPressureProfileMockCallback(mApplicationContext
                    , new BloodPressureServiceMockCallback(Objects.requireNonNull(blsServiceData), false)
                    , disServiceData == null ? null : new DeviceInformationServiceMockCallback(disServiceData, false)
                    , callbacks);
        } else {
            throw new RuntimeException("Not Found");
        }
    }

}
