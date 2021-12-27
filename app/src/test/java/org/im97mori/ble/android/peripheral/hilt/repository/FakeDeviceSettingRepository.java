package org.im97mori.ble.android.peripheral.hilt.repository;

import android.content.Context;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;

import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.CompletableOnSubscribe;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.processors.PublishProcessor;

public class FakeDeviceSettingRepository extends DeviceSettingRepository {

    public PublishProcessor<List<DeviceSetting>> mLoadAllDeviceSettingProcessor;

    public CompletableOnSubscribe mDeleteAllDeviceSettingSubscribe;

    @Inject
    public FakeDeviceSettingRepository(@NonNull DeviceSettingDataSource deviceSettingDataSource, @NonNull @ApplicationContext Context context) {
        super(deviceSettingDataSource, context);
    }

    @NonNull
    @Override
    public Single<List<DeviceSetting>> loadAllDeviceSetting() {
        return mLoadAllDeviceSettingProcessor.singleOrError();
    }

    @NonNull
    @Override
    public Completable deleteAllDeviceSetting() {
        return Completable.create(mDeleteAllDeviceSettingSubscribe);
    }

}
