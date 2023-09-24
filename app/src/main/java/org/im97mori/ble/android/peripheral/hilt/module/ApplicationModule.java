package org.im97mori.ble.android.peripheral.hilt.module;

import android.content.Context;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.BluetoothSettingRepository;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;

@Module
@InstallIn(SingletonComponent.class)
public class ApplicationModule {

    @Singleton
    @Provides
    public static BluetoothSettingRepository bindBluetoothSettingRepository(@NonNull @ApplicationContext Context context) {
        return new BluetoothSettingRepository(context);
    }

    @Singleton
    @Provides
    public static DeviceSettingRepository bindDeviceSettingRepository(@NonNull DeviceSettingDataSource deviceSettingDataSource
            , @NonNull @ApplicationContext Context context) {
        return new DeviceSettingRepository(deviceSettingDataSource, context);
    }

    @Singleton
    @Provides
    public static DeviceSettingDataSource bindDeviceSettingDataSource(@NonNull @ApplicationContext Context context) {
        return new DeviceSettingDataSource(context);
    }

}
