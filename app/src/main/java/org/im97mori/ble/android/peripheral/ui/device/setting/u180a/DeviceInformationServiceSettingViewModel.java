package org.im97mori.ble.android.peripheral.ui.device.setting.u180a;

import static org.im97mori.ble.constants.CharacteristicUUID.MANUFACTURER_NAME_STRING_CHARACTERISTIC;
import static org.im97mori.ble.constants.CharacteristicUUID.MODEL_NUMBER_STRING_CHARACTERISTIC;
import static org.im97mori.ble.constants.CharacteristicUUID.SYSTEM_ID_CHARACTERISTIC;
import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;

import android.bluetooth.BluetoothGattService;
import android.content.Intent;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseServiceSettingViewModel;
import org.im97mori.ble.characteristic.u2a23.SystemId;
import org.im97mori.ble.characteristic.u2a24.ModelNumberString;
import org.im97mori.ble.characteristic.u2a29.ManufacturerNameString;

import java.util.Optional;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class DeviceInformationServiceSettingViewModel extends BaseServiceSettingViewModel {

    private final MutableLiveData<Boolean> hasManufacturerNameString;
    private final MutableLiveData<Boolean> hasModelNumberString;
    private final MutableLiveData<Boolean> hasSystemId;
    private final MutableLiveData<Boolean> supportSystemId;

    private final MutableLiveData<String> mManufacturerNameString;
    private final MutableLiveData<String> mModelNumberString;
    private final MutableLiveData<String> mSystemId;

    private final MutableLiveData<String> manufacturerNameString;
    private final MutableLiveData<String> modelNumberString;
    private final MutableLiveData<String> manufacturerIdentifier;
    private final MutableLiveData<String> organizationallyUniqueIdentifier;

    public DeviceInformationServiceSettingViewModel(@NonNull SavedStateHandle savedStateHandle) {
        hasManufacturerNameString = savedStateHandle.getLiveData("hasManufacturerNameString");
        hasModelNumberString = savedStateHandle.getLiveData("hasModelNumberString");
        hasSystemId = savedStateHandle.getLiveData("hasSystemId");
        supportSystemId = savedStateHandle.getLiveData("supportSystemId");

        mManufacturerNameString = savedStateHandle.getLiveData("mManufacturerNameString");
        mModelNumberString = savedStateHandle.getLiveData("mModelNumberString");
        mSystemId = savedStateHandle.getLiveData("mSystemId");

        manufacturerNameString = savedStateHandle.getLiveData("manufacturerNameString");
        modelNumberString = savedStateHandle.getLiveData("modelNumberString");
        manufacturerIdentifier = savedStateHandle.getLiveData("manufacturerIdentifier");
        organizationallyUniqueIdentifier = savedStateHandle.getLiveData("organizationallyUniqueIdentifier");
    }

    @NonNull
    public Completable setup(@NonNull Intent intent) {
        Completable completable;
        if (mServiceData == null) {
            completable = Single.just(Optional.ofNullable(intent.getStringExtra(DEVICE_INFORMATION_SERVICE.toString())))
                    .subscribeOn(Schedulers.io())
                    .observeOn(Schedulers.io())
                    .flatMapCompletable(dataString -> {
                        if (dataString.isPresent()) {
                            try {
                                mServiceData = mGson.fromJson(dataString.get(), ServiceData.class);
                            } catch (JsonSyntaxException e) {
                                e.printStackTrace();
                            }
                        }

                        if (mServiceData == null) {
                            mServiceData = new ServiceData();
                            mServiceData.uuid = DEVICE_INFORMATION_SERVICE;
                            mServiceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;
                        }

                        if (hasManufacturerNameString.getValue() == null) {
                            Optional<CharacteristicData> optional = mServiceData.characteristicDataList
                                    .stream()
                                    .filter(characteristicData -> MANUFACTURER_NAME_STRING_CHARACTERISTIC.equals(characteristicData.uuid))
                                    .findAny();
                            hasManufacturerNameString.postValue(optional.isPresent());
                            if (optional.isPresent()) {
                                CharacteristicData characteristicData = optional.get();
                                if (mManufacturerNameString.getValue() == null) {
                                    mManufacturerNameString.postValue(mGson.toJson(characteristicData));
                                }
                                if (manufacturerNameString.getValue() == null && characteristicData.data != null) {
                                    manufacturerNameString.postValue(new ManufacturerNameString(characteristicData.data).getManufacturerName());
                                }
                            }
                        }

                        if (hasModelNumberString.getValue() == null) {
                            Optional<CharacteristicData> optional = mServiceData.characteristicDataList
                                    .stream()
                                    .filter(characteristicData -> MODEL_NUMBER_STRING_CHARACTERISTIC.equals(characteristicData.uuid))
                                    .findAny();
                            hasModelNumberString.postValue(optional.isPresent());
                            if (optional.isPresent()) {
                                CharacteristicData characteristicData = optional.get();
                                if (mModelNumberString.getValue() == null) {
                                    mModelNumberString.postValue(mGson.toJson(characteristicData));
                                }
                                if (modelNumberString.getValue() == null && characteristicData.data != null) {
                                    modelNumberString.postValue(new ModelNumberString(characteristicData.data).getModelNumber());
                                }
                            }
                        }

                        Optional<CharacteristicData> optional = mServiceData.characteristicDataList
                                .stream()
                                .filter(characteristicData -> SYSTEM_ID_CHARACTERISTIC.equals(characteristicData.uuid))
                                .findAny();
                        if (hasSystemId.getValue() == null) {
                            hasSystemId.postValue(optional.isPresent());
                            if (optional.isPresent()) {
                                CharacteristicData characteristicData = optional.get();
                                if (mSystemId.getValue() == null) {
                                    mSystemId.postValue(mGson.toJson(characteristicData));
                                }
                                if (characteristicData.data != null) {
                                    SystemId systemId = new SystemId(characteristicData.data);
                                    if (manufacturerIdentifier.getValue() == null) {
                                        manufacturerIdentifier.postValue(String.valueOf(systemId.getManufacturerIdentifier()));
                                    }
                                    if (organizationallyUniqueIdentifier.getValue() == null) {
                                        organizationallyUniqueIdentifier.postValue(String.valueOf(systemId.getOrganizationallyUniqueIdentifier()));
                                    }
                                }
                            }
                        }

                        if (supportSystemId.getValue() == null) {
                            supportSystemId.postValue(optional.isPresent());
                        }

                        return Completable.complete();
                    });
        } else {
            completable = Completable.complete();
        }
        return completable;
    }

    public void observeHasManufacturerNameString(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasManufacturerNameString).observe(owner, observer);
    }

    public void observeHasModelNumberString(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasModelNumberString).observe(owner, observer);
    }

    public void observeHasSystemId(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasSystemId).observe(owner, observer);
    }

    public void observeSupportSystemId(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(supportSystemId).observe(owner, observer);
    }

    public void observeManufacturerNameString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(manufacturerNameString).observe(owner, observer);
    }

    public void observeModelNumberString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(modelNumberString).observe(owner, observer);
    }

    public void observeManufacturerIdentifier(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(manufacturerIdentifier).observe(owner, observer);
    }

    public void observeOrganizationallyUniqueIdentifier(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(organizationallyUniqueIdentifier).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateSupportSystemId(@NonNull Boolean checked) {
        supportSystemId.setValue(checked);
        if (!checked && Boolean.TRUE.equals(hasSystemId.getValue())) {
            hasSystemId.setValue(false);
        }
    }

    @Nullable
    public String getManufacturerNameStringCharacteristicDataString() {
        String dataString = null;
        Optional<CharacteristicData> result = mServiceData.characteristicDataList
                .stream()
                .filter(characteristicData -> characteristicData.uuid.equals(MANUFACTURER_NAME_STRING_CHARACTERISTIC))
                .findFirst();
        if (result.isPresent()) {
            dataString = mGson.toJson(new ManufacturerNameString(result.get().data));
        }
        return dataString;
    }

    @MainThread
    public void setManufacturerNameStringCharacteristicDataString(@Nullable String manufacturerNameStringDataString) {
        mManufacturerNameString.setValue(manufacturerNameStringDataString);
        if (manufacturerNameStringDataString == null) {
            hasManufacturerNameString.setValue(false);
            manufacturerNameString.setValue(null);
        } else {
            try {
                CharacteristicData characteristicData = mGson.fromJson(manufacturerNameStringDataString, CharacteristicData.class);
                hasManufacturerNameString.setValue(true);
                manufacturerNameString.setValue(new ManufacturerNameString(characteristicData.data).getManufacturerName());
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @Nullable
    public String getModelNumberStringCharacteristicDataString() {
        String dataString = null;
        Optional<CharacteristicData> result = mServiceData.characteristicDataList
                .stream()
                .filter(characteristicData -> characteristicData.uuid.equals(MODEL_NUMBER_STRING_CHARACTERISTIC))
                .findFirst();
        if (result.isPresent()) {
            dataString = mGson.toJson(new ManufacturerNameString(result.get().data));
        }
        return dataString;
    }

    @MainThread
    public void setModelNumberStringCharacteristicDataString(@Nullable String modelNumberStringDataString) {
        mModelNumberString.setValue(modelNumberStringDataString);
        if (modelNumberStringDataString == null) {
            hasModelNumberString.setValue(false);
            modelNumberString.setValue(null);
        } else {
            try {
                CharacteristicData characteristicData = mGson.fromJson(modelNumberStringDataString, CharacteristicData.class);
                hasModelNumberString.setValue(true);
                modelNumberString.setValue(new ModelNumberString(characteristicData.data).getModelNumber());
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @Nullable
    public String getSystemIdCharacteristicDataString() {
        String dataString = null;
        Optional<CharacteristicData> result = mServiceData.characteristicDataList
                .stream()
                .filter(characteristicData -> characteristicData.uuid.equals(SYSTEM_ID_CHARACTERISTIC))
                .findFirst();
        if (result.isPresent()) {
            dataString = mGson.toJson(new SystemId(result.get().data));
        }
        return dataString;
    }

    @MainThread
    public void setSystemIdCharacteristicDataString(@Nullable String systemIdDataString) {
        mSystemId.setValue(systemIdDataString);
        if (systemIdDataString == null) {
            hasSystemId.setValue(false);
            manufacturerIdentifier.setValue(null);
            organizationallyUniqueIdentifier.setValue(null);
        } else {
            try {
                CharacteristicData characteristicData = mGson.fromJson(systemIdDataString, CharacteristicData.class);
                hasSystemId.setValue(true);
                SystemId systemId = new SystemId(characteristicData.data);
                manufacturerIdentifier.setValue(String.valueOf(systemId.getManufacturerIdentifier()));
                organizationallyUniqueIdentifier.setValue(String.valueOf(systemId.getOrganizationallyUniqueIdentifier()));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @NonNull
    @Override
    public Single<Optional<Intent>> save() {
        Intent intent;
        mServiceData.characteristicDataList.clear();
        if (Boolean.TRUE.equals(hasManufacturerNameString.getValue())) {
            try {
                mServiceData.characteristicDataList.add(mGson.fromJson(mManufacturerNameString.getValue(), CharacteristicData.class));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
        if (Boolean.TRUE.equals(hasModelNumberString.getValue())) {
            try {
                mServiceData.characteristicDataList.add(mGson.fromJson(mModelNumberString.getValue(), CharacteristicData.class));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
        if (Boolean.TRUE.equals(hasSystemId.getValue())) {
            try {
                mServiceData.characteristicDataList.add(mGson.fromJson(mSystemId.getValue(), CharacteristicData.class));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }

        if (mServiceData.characteristicDataList
                .stream()
                .filter(characteristicData -> !characteristicData.uuid.equals(SYSTEM_ID_CHARACTERISTIC)).count() == 2) {
            intent = new Intent();
            intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), mGson.toJson(mServiceData));
        } else {
            intent = null;
        }
        return Single.just(Optional.ofNullable(intent));
    }

}