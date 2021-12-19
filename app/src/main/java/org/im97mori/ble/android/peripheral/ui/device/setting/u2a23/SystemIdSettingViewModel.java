package org.im97mori.ble.android.peripheral.ui.device.setting.u2a23;

import static org.im97mori.ble.constants.CharacteristicUUID.SYSTEM_ID_CHARACTERISTIC;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Intent;
import android.text.TextUtils;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseCharacteristicViewModel;
import org.im97mori.ble.characteristic.u2a23.SystemId;

import java.util.Objects;
import java.util.Optional;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class SystemIdSettingViewModel extends BaseCharacteristicViewModel {

    private final MutableLiveData<Boolean> isErrorResponse;

    private final MutableLiveData<String> manufacturerIdentifier;
    private final MutableLiveData<String> manufacturerIdentifierError;
    private final MutableLiveData<String> organizationallyUniqueIdentifier;
    private final MutableLiveData<String> organizationallyUniqueIdentifierError;
    private final MutableLiveData<String> responseDelay;
    private final MutableLiveData<String> responseDelayError;
    private final MutableLiveData<String> responseCode;
    private final MutableLiveData<String> responseCodeError;

    public SystemIdSettingViewModel(@NonNull SavedStateHandle savedStateHandle) {
        isErrorResponse = savedStateHandle.getLiveData("isErrorResponse");
        manufacturerIdentifier = savedStateHandle.getLiveData("manufacturerIdentifier");
        manufacturerIdentifierError = savedStateHandle.getLiveData("manufacturerIdentifierError");
        organizationallyUniqueIdentifier = savedStateHandle.getLiveData("organizationallyUniqueIdentifier");
        organizationallyUniqueIdentifierError = savedStateHandle.getLiveData("organizationallyUniqueIdentifierError");
        responseDelay = savedStateHandle.getLiveData("responseDelay");
        responseDelayError = savedStateHandle.getLiveData("responseDelayError");
        responseCode = savedStateHandle.getLiveData("responseCode");
        responseCodeError = savedStateHandle.getLiveData("responseCodeError");
    }

    @NonNull
    public Completable setup(@NonNull Intent intent) {
        Completable completable;
        if (mCharacteristicData == null) {
            completable = Single.just(Optional.ofNullable(intent.getStringExtra(SYSTEM_ID_CHARACTERISTIC.toString())))
                    .subscribeOn(Schedulers.io())
                    .observeOn(Schedulers.io())
                    .flatMapCompletable(dataString -> {
                        if (dataString.isPresent()) {
                            try {
                                mCharacteristicData = mGson.fromJson(dataString.get(), CharacteristicData.class);
                            } catch (JsonSyntaxException e) {
                                e.printStackTrace();
                            }
                        }

                        if (mCharacteristicData == null) {
                            mCharacteristicData = new CharacteristicData();
                            mCharacteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
                            mCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
                            mCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
                        }

                        if (isErrorResponse.getValue() == null) {
                            isErrorResponse.postValue(mCharacteristicData.responseCode != BluetoothGatt.GATT_SUCCESS);
                        }

                        String text;
                        if (mCharacteristicData.data != null) {
                            SystemId systemId = new SystemId(mCharacteristicData.data);
                            text = String.valueOf(systemId.getManufacturerIdentifier());
                            if (this.manufacturerIdentifier.getValue() == null) {
                                this.manufacturerIdentifier.postValue(text);
                            }
                            manufacturerIdentifierError.postValue(mResourceTextSource.getManufacturerIdentifierErrorString(text));

                            text = String.valueOf(systemId.getOrganizationallyUniqueIdentifier());
                            if (this.organizationallyUniqueIdentifier.getValue() == null) {
                                this.organizationallyUniqueIdentifier.postValue(text);
                            }
                            organizationallyUniqueIdentifierError.postValue(mResourceTextSource.getOrganizationallyUniqueIdentifierErrorString(text));
                        } else {
                            manufacturerIdentifierError.postValue(mResourceTextSource.getManufacturerNameStringErrorString(this.manufacturerIdentifier.getValue()));
                            organizationallyUniqueIdentifierError.postValue(mResourceTextSource.getManufacturerNameStringErrorString(this.organizationallyUniqueIdentifier.getValue()));
                        }

                        text = String.valueOf(mCharacteristicData.delay);
                        if (responseDelay.getValue() == null) {
                            responseDelay.postValue(text);
                            responseDelayError.postValue(mResourceTextSource.getResponseDelayErrorString(text));
                        }

                        text = String.valueOf(mCharacteristicData.responseCode);
                        if (responseCode.getValue() == null) {
                            responseCode.postValue(text);
                            responseCodeError.postValue(mResourceTextSource.getResponseCodeErrorString(text));
                        }

                        return Completable.complete();
                    });
        } else {
            completable = Completable.complete();
        }
        return completable;
    }

    public void observeIsErrorResponse(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(isErrorResponse).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateIsErrorResponse(@NonNull Boolean checked) {
        isErrorResponse.setValue(checked);
    }

    public void observeManufacturerIdentifier(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(manufacturerIdentifier).observe(owner, observer);
    }

    public void observeManufacturerIdentifierError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(manufacturerIdentifierError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateManufacturerIdentifier(@NonNull CharSequence text) {
        manufacturerIdentifier.setValue(text.toString());
        manufacturerIdentifierError.setValue(mResourceTextSource.getManufacturerIdentifierErrorString(text));
    }

    public void observeOrganizationallyUniqueIdentifier(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(organizationallyUniqueIdentifier).observe(owner, observer);
    }

    public void observeOrganizationallyUniqueIdentifierError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(organizationallyUniqueIdentifierError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateOrganizationallyUniqueIdentifier(@NonNull CharSequence text) {
        organizationallyUniqueIdentifier.setValue(text.toString());
        organizationallyUniqueIdentifierError.setValue(mResourceTextSource.getOrganizationallyUniqueIdentifierErrorString(text));
    }

    public void observeResponseDelay(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(responseDelay).observe(owner, observer);
    }

    public void observeResponseDelayError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(responseDelayError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateResponseDelay(@NonNull CharSequence text) {
        responseDelay.setValue(text.toString());
        responseDelayError.setValue(mResourceTextSource.getResponseDelayErrorString(text));
    }

    public void observeResponseCode(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(responseCode).observe(owner, observer);
    }

    public void observeResponseCodeError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(responseCodeError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateResponseCode(@NonNull CharSequence text) {
        responseCode.setValue(text.toString());
        responseCodeError.setValue(mResourceTextSource.getResponseCodeErrorString(text));
    }

    @NonNull
    @Override
    public Single<Optional<Intent>> save() {
        Intent intent;
        if (TextUtils.isEmpty(manufacturerIdentifierError.getValue())
                && TextUtils.isEmpty(organizationallyUniqueIdentifierError.getValue())
                && (Boolean.TRUE.equals(isErrorResponse.getValue())
                && TextUtils.isEmpty(responseCodeError.getValue())
                || Boolean.FALSE.equals(isErrorResponse.getValue()))
                && TextUtils.isEmpty(responseDelayError.getValue())) {
            mCharacteristicData.data = new SystemId(Long.parseLong(Objects.requireNonNull(manufacturerIdentifier.getValue()))
                    , Integer.parseInt(Objects.requireNonNull(organizationallyUniqueIdentifier.getValue()))).getBytes();
            if (Boolean.TRUE.equals(isErrorResponse.getValue())) {
                mCharacteristicData.responseCode = Integer.parseInt(Objects.requireNonNull(responseCode.getValue()));
            } else {
                mCharacteristicData.responseCode = BluetoothGatt.GATT_SUCCESS;
            }
            mCharacteristicData.delay = Long.parseLong(Objects.requireNonNull(responseDelay.getValue()));

            intent = new Intent();
            intent.putExtra(SYSTEM_ID_CHARACTERISTIC.toString(), mGson.toJson(mCharacteristicData));
        } else {
            intent = null;
        }
        return Single.just(Optional.ofNullable(intent));
    }
}