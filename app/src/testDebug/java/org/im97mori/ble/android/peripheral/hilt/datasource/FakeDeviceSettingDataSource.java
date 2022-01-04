package org.im97mori.ble.android.peripheral.hilt.datasource;

import android.content.Context;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.room.DeviceSetting;

import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.CompletableOnSubscribe;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.processors.PublishProcessor;

public class FakeDeviceSettingDataSource extends DeviceSettingDataSource {

    public PublishProcessor<List<DeviceSetting>> mLoadAllDeviceSettingProcessor;

    public PublishProcessor<DeviceSetting> mLoadDeviceSettingByIdProcessor;

    public CompletableOnSubscribe mInsertDeviceSettingSubscribe;

    public CompletableOnSubscribe mDeleteDeviceSettingSubscribe;

    public CompletableOnSubscribe mDeleteAllDeviceSettingSubscribe;

    @Inject
    public FakeDeviceSettingDataSource(@NonNull @ApplicationContext Context context) {
        super(context);
    }

    @NonNull
    @Override
    public Flowable<List<DeviceSetting>> loadAllDeviceSetting() {
        return Flowable.fromPublisher(mLoadAllDeviceSettingProcessor);
    }

    @NonNull
    @Override
    public Single<DeviceSetting> loadDeviceSettingById(long id) {
        return Single.fromPublisher(mLoadDeviceSettingByIdProcessor);
    }

    @NonNull
    @Override
    public Completable insertDeviceSetting(@NonNull DeviceSetting deviceSetting) {
        return Completable.create(mInsertDeviceSettingSubscribe);
    }

    @NonNull
    @Override
    public Completable deleteDeviceSetting(@NonNull DeviceSetting deviceSetting) {
        return Completable.create(mDeleteDeviceSettingSubscribe);
    }

    @NonNull
    @Override
    public Completable deleteAllDeviceSetting() {
        return Completable.create(mDeleteAllDeviceSettingSubscribe);
    }

}
